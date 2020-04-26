const std = @import("std");

fn RaxNode(comptime V: type) type {
    return struct {
        key: ?V,
        layout: union(enum) {
            Compressed: struct {
                chars: []u8,
                next: *Self,
            },
            Branching: []Branch,
        },

        const Self = @This();
        const Branch = struct {
            char: u8,
            child: *Self,
        };

        fn initCompressedNode(allocator: *std.mem.Allocator, chars: []const u8, next: *Self) !*Self {
            const charsCopy = try allocator.alloc(u8, chars.len);
            errdefer allocator.free(charsCopy);
            std.mem.copy(u8, charsCopy, chars);

            var newNode = try allocator.create(Self);
            newNode.* = .{
                .key = null,
                .layout = .{
                    .Compressed = .{
                        .chars = charsCopy,
                        .next = next,
                    },
                },
            };
            return newNode;
        }

        fn initBranchNode(comptime N: comptime_int, allocator: *std.mem.Allocator, chars: [N]u8, children: [N]*Self) !*Self {
            var branches = try allocator.alloc(Self.Branch, N);
            errdefer allocator.free(branches);

            comptime var i: usize = 0;
            inline while (i < N) : (i += 1) {
                branches[i] = .{
                    .char = chars[i],
                    .child = children[i],
                };
            }

            var newNode = try allocator.create(Self);
            newNode.* = .{
                .key = null,
                .layout = .{
                    .Branching = branches,
                },
            };

            return newNode;
        }

        fn initEmpty(allocator: *std.mem.Allocator) !*Self {
            var result = try allocator.create(Self);
            result.* = Self{
                .key = null,
                .layout = .{ .Branching = &[0]Branch{} },
            };
            return result;
        }

        fn deinit(self: *Self, allocator: *std.mem.Allocator, deletedNodes: u64) u64 {
            var newDeletedNodes = 1 + deletedNodes;
            switch (self.layout) {
                .Compressed => |c| {
                    allocator.free(c.chars);
                    newDeletedNodes += c.next.deinit(allocator, deletedNodes);
                },
                .Branching => |branches| {
                    for (branches) |*b| newDeletedNodes += b.child.deinit(allocator, deletedNodes);
                    allocator.free(branches);
                },
            }
            allocator.destroy(self);
            return newDeletedNodes;
        }

        fn makeCompressed(self: *Self, allocator: *std.mem.Allocator, s: []const u8) !void {
            if (self.layout == .Compressed or self.layout.Branching.len != 0) {
                @panic("tried to call makeCompressed on an unsuitable node");
            }

            const charsSlice = try allocator.alloc(u8, s.len);
            errdefer allocator.free(charsSlice);

            std.mem.copy(u8, charsSlice, s);

            const nextChild = try allocator.create(Self);
            errdefer allocator.destroy(nextChild);

            // Initialize the new child
            nextChild.* = Self{
                .key = null,
                .layout = .{ .Branching = &[0]Branch{} },
            };

            // Set the current node to Compressed
            self.layout = .{
                .Compressed = .{
                    .chars = charsSlice,
                    .next = nextChild,
                },
            };
        }
    };
}

pub fn Rax(comptime V: type) type {
    const NodeT = comptime RaxNode(V);

    return struct {
        allocator: *std.mem.Allocator,
        head: *NodeT,
        numElements: u64,
        numNodes: u64,

        const Self = @This();
        pub fn init(allocator: *std.mem.Allocator) !Self {
            return Self{
                .allocator = allocator,
                .head = try NodeT.initEmpty(allocator),
                .numNodes = 1,
                .numElements = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            const deletedNodes = self.head.deinit(self.allocator, 0);
            if (self.numNodes != deletedNodes) {
                std.debug.warn("numnodes = {} deleted = {}", .{ self.numNodes, deletedNodes });
                @panic("free didn't free!");
            }
        }

        pub const InsertResult = union(enum) {
            New,
            Replaced: V,
        };

        pub fn insert(self: *Self, key: []const u8, value: V) !InsertResult {
            return insertImpl(self, key, value, false);
        }
        pub fn insertOverride(self: *Self, key: []const u8, value: V) !InsertResult {
            return insertImpl(self, key, value, true);
        }

        // TODO: override is not comptime. Should it be?
        fn insertImpl(self: *Self, key: []const u8, value: V, override: bool) !InsertResult {
            var wk = self.lowWalk(key);

            var i = wk.bytesMatched;
            var currentNode = wk.stopNode;
            var j = wk.splitPos;

            // /* If i == len we walked following the whole string. If we are not
            // * in the middle of a compressed node, the string is either already
            // * inserted or this middle node is currently not a key, but can represent
            // * our key. We have just to reallocate the node and make space for the
            // * data pointer. */
            if (i == key.len and
                (currentNode.layout != .Compressed or wk.splitPos == 0))
            {
                if (currentNode.key) |oldValue| {
                    if (override) {
                        currentNode.key = value;
                    }
                    return InsertResult{ .Replaced = oldValue };
                } else {
                    currentNode.key = value;
                    self.numElements += 1;
                    return .New;
                }
            }

            // Algo 1
            if (currentNode.layout == .Compressed and i != key.len) {
                var currentData = &currentNode.layout.Compressed;

                // We are breaking up the current node in two parts.
                // We will need to create a branching node for the point of devergence,
                // and other nodes to make space for the new suffixes (the old suffix
                // from this node, and the new suffix for the remaining chars in key).
                // We start by creating the suffix nodes (+ eventual extra emtpy nodes),
                // and at the end of the procedure we create the branching node that they
                // will have to be connected to.

                // Create the node containing the current node's suffix (skipping j,
                // the char that will be put in a branching node).
                var oldSuffixNode = try self.createSuffixForOldElement(currentNode, j + 1);

                // Create an empty node for the new suffix. It will be populated by
                // the node downstream (the while loop).
                var newSuffixNode = try NodeT.initEmpty(self.allocator);

                // Create the branching node for the split-off point and connect
                // the nodes we previously created to it.
                var branchingNode: *NodeT = undefined;
                var newSuffixSlot: usize = undefined;
                {
                    var oldSuffixSlot: usize = undefined;
                    // We must maintain lexicographial ordering for the branches in
                    // the new branching node.
                    if (currentData.chars[j] < key[i]) {
                        oldSuffixSlot = 0;
                        newSuffixSlot = 1;
                    } else {
                        oldSuffixSlot = 1;
                        newSuffixSlot = 0;
                    }

                    var chars: [2]u8 = undefined;
                    chars[oldSuffixSlot] = currentData.chars[j];
                    chars[newSuffixSlot] = key[i];

                    var children: [2]*NodeT = undefined;
                    children[oldSuffixSlot] = oldSuffixNode;
                    children[newSuffixSlot] = newSuffixNode;

                    // If we are splitting right at the beginning, change the current node
                    // to be the branching node, otherwise allocate a new one.
                    if (j == 0) {
                        var branches = try self.allocator.alloc(NodeT.Branch, 2);
                        branches[oldSuffixSlot] = .{
                            .char = chars[oldSuffixSlot],
                            .child = children[oldSuffixSlot],
                        };
                        branches[newSuffixSlot] = .{
                            .char = chars[newSuffixSlot],
                            .child = children[newSuffixSlot],
                        };

                        branchingNode = currentNode;
                        branchingNode.layout = .{
                            .Branching = branches,
                        };
                    } else {
                        // Create the split node (the split node has a .Branching layout)
                        branchingNode = try NodeT.initBranchNode(2, self.allocator, chars, children);
                        self.numNodes += 1;
                    }

                    // The rest of the algorithm will add the actual new key suffix node.
                    // In this segment we only setup the branching part.
                }

                if (j > 0) {
                    // Trim the current node by removing the suffix + branching point and
                    // set nextPtr to the new branching node just created.
                    try self.trimCommonPrefixNode(currentNode, j, branchingNode);
                }

                // Move the 'currentNode' reference to the new empty node created for
                // the newSuffix and let the next code to deal with it.
                currentNode = branchingNode.layout.Branching[newSuffixSlot].child;
                i += 1;
            } else if (currentNode.layout == .Compressed and i == key.len) {
                var currentData = &currentNode.layout.Compressed;

                // Child containing the current node's suffix.
                var oldSuffixNode = try self.createSuffixForOldElement(currentNode, j);
                try self.trimCommonPrefixNode(currentNode, j, oldSuffixNode);
                currentNode = oldSuffixNode;
                i = key.len;
            }

            while (i < key.len) {
                const isEmpty = switch (currentNode.layout) {
                    .Compressed => |c| c.chars.len == 0,
                    .Branching => |b| b.len == 0,
                };

                if (isEmpty and key.len - i > 1) {
                    var chunkSize = key.len - i;

                    // this should be maxsize of u29 in the
                    // real implementation.
                    if (chunkSize > std.math.maxInt(usize)) {
                        chunkSize = std.math.maxInt(usize);
                    }

                    // TODO: error handling procedure
                    try currentNode.makeCompressed(self.allocator, key[i..]);
                    self.numNodes += 1;
                    i += chunkSize;
                    currentNode = currentNode.layout.Compressed.next;
                } else {
                    const emptyNode = try NodeT.initEmpty(self.allocator);
                    self.numNodes += 1;

                    errdefer {
                        _ = emptyNode.deinit(self.allocator, 0);
                    }

                    const oldBranches = currentNode.layout.Branching;
                    var newBranches = try self.allocator.alloc(NodeT.Branch, oldBranches.len + 1);

                    var greaterThan = newBranches.len - 1;
                    for (oldBranches) |b, x| {
                        if (b.char > key[x]) {
                            greaterThan = x;
                            break;
                        }
                    }

                    // TODO: break this up into 2 loops without ifs in them.
                    for (newBranches) |*b, idx| {
                        if (idx < greaterThan) {
                            b.* = oldBranches[idx];
                        }
                        if (idx == greaterThan) {
                            newBranches[idx] = .{
                                .char = key[i],
                                .child = emptyNode,
                            };
                        }
                        if (idx > greaterThan) {
                            b.* = oldBranches[idx - 1];
                        }
                    }

                    self.allocator.free(oldBranches);
                    currentNode.layout.Branching = newBranches;
                    i += 1;
                    currentNode = emptyNode;
                }
            }

            if (currentNode.key == null) {
                self.numElements += 1;
            }

            currentNode.key = value;
            return .New;
        }

        fn createSuffixForOldElement(self: *Self, currentNode: *NodeT, startPos: usize) !*NodeT {
            var oldSuffixNode: *NodeT = undefined;
            var currentData = &currentNode.layout.Compressed;
            // If there's a suffix that we broke off from the original node,
            // we need to allocate a new compressed node for it.
            const oldSuffixLen = currentData.chars.len - startPos;
            if (oldSuffixLen > 0) { // At least 1 char in suffix.
                if (oldSuffixLen == 1) {
                    // Single-char suffix means we create a branching child node
                    // for it and then attach the current child as it's child
                    // corresponding to the only present branch
                    oldSuffixNode = try NodeT.initBranchNode(
                        1,
                        self.allocator,
                        [1]u8{currentData.chars[startPos]},
                        [1]*NodeT{currentData.next},
                    );
                    self.numNodes += 1;
                } else {
                    // Multiple chars in the suffix -> create compressed node.
                    oldSuffixNode = try NodeT.initCompressedNode(
                        self.allocator,
                        currentData.chars[startPos..],
                        currentData.next,
                    );
                    self.numNodes += 1;
                }
            } else {
                // No suffix, we branched at the last char, so the child
                // corresponding to this char is directly the old child (next)
                oldSuffixNode = currentData.next;
            }

            return oldSuffixNode;
        }

        fn trimCommonPrefixNode(self: *Self, currentNode: *NodeT, j: usize, nextNode: *NodeT) !void {
            var currentData = &currentNode.layout.Compressed;
            if (j == 1) {
                const firstLetter = currentData.chars[0];
                self.allocator.free(currentData.chars);

                var branches = try self.allocator.alloc(NodeT.Branch, 1);
                branches[0] = .{
                    .char = firstLetter,
                    .child = nextNode,
                };

                // Change the layout of the prefix node to Branching
                currentNode.layout = .{ .Branching = branches };
            } else {
                currentData.chars = self.allocator.shrink(currentData.chars, j);
                currentData.next = nextNode;
            }
        }

        // TODO: should s be named key? Do we ever search
        // for "partials"?
        // TODO: is returning a struct better or worse than
        //       writing to pointers?
        const walkResult = struct {
            bytesMatched: usize,
            stopNode: *NodeT,
            splitPos: usize, // In real impl it's u29
        };

        fn lowWalk(self: *Self, s: []const u8) walkResult {
            var node = self.head;

            var i: usize = 0;
            var j: usize = 0;
            while (i < s.len) {
                switch (node.layout) {
                    .Compressed => |data| {
                        if (data.chars.len == 0) break;

                        j = 0;
                        while (j < data.chars.len and i < s.len) {
                            if (data.chars[j] != s[i]) break;
                            j += 1;
                            i += 1;
                        }
                        if (j != data.chars.len) break;
                    },
                    .Branching => |branches| {
                        if (branches.len == 0) break;

                        // rax does a linear scan, so we do too.
                        while (j < branches.len and i < s.len) : (j += 1) {
                            if (branches[j].char == s[i]) break;
                        }
                        if (j == branches.len) break;
                        i += 1;
                    },
                }

                node = switch (node.layout) {
                    .Compressed => |c| c.next,
                    .Branching => |b| b[j].child,
                };

                j = 0;
            }

            return walkResult{
                .bytesMatched = i,
                .stopNode = node,
                .splitPos = j,
            };
        }

        pub fn show(self: *Self) void {
            recursiveShow(0, 0, self.head);
            std.debug.warn("\n", .{});
        }

        fn recursiveShow(level: usize, lp: usize, node: *NodeT) void {
            var lpad = lp;
            const isCompressed = node.layout == .Compressed;
            const start: u8 = if (isCompressed) '"' else '[';
            const end: u8 = if (isCompressed) '"' else ']';

            var numchars: usize = 2;
            switch (node.layout) {
                .Compressed => |compressed| {
                    std.debug.warn("{c}{}{c}", .{ start, compressed.chars, end });
                    numchars += compressed.chars.len;
                },
                .Branching => |branches| {
                    std.debug.warn("{c}", .{start});
                    for (branches) |b| {
                        std.debug.warn("{c}", .{b.char});
                    }
                    std.debug.warn("{c}", .{end});
                    numchars += branches.len;
                },
            }

            if (node.key) |value| {
                if (@TypeOf(value) != void) {
                    std.debug.warn("={}", .{value});
                }
                numchars += 4;
            }

            if (level > 0) {
                lpad += switch (node.layout) {
                    .Compressed => @as(usize, 4),
                    .Branching => |b| switch (b.len) {
                        0 => @as(usize, 4),
                        1 => @as(usize, 4) + numchars,
                        else => @as(usize, 7),
                    },
                };
            }

            switch (node.layout) {
                .Compressed => |compressed| {
                    std.debug.warn(" -> ", .{});
                    recursiveShow(level + 1, lpad, compressed.next);
                },
                .Branching => |branches| {
                    for (branches) |*br, idx| {
                        if (branches.len > 1) {
                            std.debug.warn("\n", .{});
                            var i: usize = 0;
                            while (i < lpad) : (i += 1) std.debug.warn(" ", .{});
                            std.debug.warn(" `-({c}) ", .{br.char});
                        } else {
                            std.debug.warn(" -> ", .{});
                        }
                        recursiveShow(level + 1, lpad, br.child);
                    }
                },
            }
        }
    };
}

test "works" {
    const MyRax = Rax(void);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();
}

test "try simple insert" {
    const MyRax = Rax(void);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();

    _ = try r.insert("banana", {});
}
