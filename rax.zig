const std = @import("std");

fn RaxNode(comptime V: type) type {
    return struct {
        nodeType: union(enum) {
            Key: V,
            Link,
        },
        layout: union(enum) {
            Compressed: struct {
                chars: []u8,
                nextPtr: *Self,
            },
            Branching: []Branch,
        },

        const Self = @This();
        const Branch = struct {
            char: u8,
            child: Self,
        };

        fn init(allocator: *std.mem.Allocator, children: usize) !Self {
            const branchSlice = try allocator.alloc(Branch, children);
            errdefer allocator.free(branchSlice);

            return Self{
                .nodeType = .Link,
                .layout = .{ .Branching = branchSlice },
            };
        }

        fn deinit(self: *Self, allocator: *std.mem.Allocator, deletedNodes: u64) u64 {
            var newDeletedNodes = deletedNodes;
            switch (self.layout) {
                .Compressed => |c| {
                    allocator.free(c.chars);
                    newDeletedNodes += c.nextPtr.deinit(allocator, deletedNodes);
                    allocator.destroy(c.nextPtr);
                    newDeletedNodes += 1;
                },
                .Branching => |branches| {
                    for (branches) |*b| newDeletedNodes += b.child.deinit(allocator, deletedNodes);
                    allocator.free(branches);
                },
            }
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
                .nodeType = .Link,
                .layout = .{ .Branching = &[0]Branch{} },
            };

            // Set the current node to Compressed
            self.layout = .{
                .Compressed = .{
                    .chars = charsSlice,
                    .nextPtr = nextChild,
                },
            };
        }
    };
}

pub fn Rax(comptime V: type) type {
    const NodeT = comptime RaxNode(V);

    return struct {
        allocator: *std.mem.Allocator,
        head: NodeT,
        numElements: u64,
        numNodes: u64,

        const Self = @This();
        pub fn init(allocator: *std.mem.Allocator) !Self {
            return Self{
                .allocator = allocator,
                .head = try NodeT.init(allocator, 0),
                .numNodes = 1,
                .numElements = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            const deletedNodes = 1 + self.head.deinit(self.allocator, 0);
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
            // var parentLink = wk.pLink;

            // /* If i == len we walked following the whole string. If we are not
            // * in the middle of a compressed node, the string is either already
            // * inserted or this middle node is currently not a key, but can represent
            // * our key. We have just to reallocate the node and make space for the
            // * data pointer. */
            if (i == key.len and
                (currentNode.layout != .Compressed or wk.splitPos == 0))
            {
                switch (currentNode.nodeType) {
                    .Link => {
                        currentNode.nodeType = .{ .Key = value };
                        self.numElements += 1;
                        return .New;
                    },
                    .Key => |oldValue| {
                        if (override) {
                            currentNode.nodeType = .{ .Key = value };
                        }
                        return InsertResult{ .Replaced = oldValue };
                    },
                }
            }

            // Algo 1
            if (currentNode.layout == .Compressed and i != key.len) {
                var currentData = &currentNode.layout.Compressed;
                if (j == currentData.chars.len - 1 or j == key.len - 1) {
                    @panic("j shouldn't be the last char");
                }

                // ----------------

                // Create the split node (the split node has a .Branching layout)
                var branchingNodePtr = try self.allocator.create(NodeT);
                branchingNodePtr.* = try NodeT.init(self.allocator, 2);
                self.numNodes += 1;
                // errdefer branchingNode.deinit();
                // TODO: error handling

                // Branch to the child containing the current node's suffix.
                {
                    // Create the node that contains the suffix of this compressed node
                    // that we are breaking up. This new node will then point (.nextPtr) to
                    // the current child of this node.
                    const suffixLen = currentData.chars.len - j;
                    const suffix = try self.allocator.alloc(u8, suffixLen);
                    errdefer self.allocator.free(suffix);

                    // Copy the string suffix over the new array
                    // We do +1 because `j` points to the char where the two strings
                    // diverge and that char will go in another intermediary branching
                    // node.
                    std.mem.copy(u8, suffix, currentData.chars[j..]);
                    branchingNodePtr.layout.Branching[0] = NodeT.Branch{
                        .char = currentData.chars[j],
                        .child = .{
                            .nodeType = .Link, // TODO check
                            .layout = .{
                                .Compressed = .{
                                    .chars = suffix,
                                    .nextPtr = currentData.nextPtr,
                                },
                            },
                        },
                    };
                }

                // Branch containing the new suffix that we are creating for key.
                {
                    // const suffixLen = key.len - j;
                    // const suffix = try self.allocator.alloc(u8, suffixLen);
                    // errdefer self.allocator.free(suffix);

                    // Copy the string suffix over the new array
                    // We do +1 because `j` points to the char where the two strings
                    // diverge and that char will go in another intermediary branching
                    // node.
                    // std.mem.copy(u8, suffix, key[j..]);
                    branchingNodePtr.layout.Branching[1] = NodeT.Branch{
                        .char = key[j],
                        .child = .{
                            .nodeType = .Link,
                            .layout = .{
                                .Branching = &[0]NodeT.Branch{},
                            },
                        },
                    };
                    // TODO: is this always going to be like this?
                    // try branchingNodePtr.layout.Branching[1].child.makeCompressed(self.allocator, suffix);
                    // self.numNodes += 1;
                }

                // Trim the current node by removing the suffix + branching point and
                // set nextPtr to the new branching node just created.
                currentData.chars = self.allocator.shrink(currentData.chars, j);
                currentData.nextPtr = branchingNodePtr;
                self.numElements += 1;
                currentNode = &branchingNodePtr.layout.Branching[1].child;
                i += 1;
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

                    i += chunkSize;
                } else {
                    std.debug.warn("char={c} node={}\n", .{ key[i], currentNode.* });
                    unreachable;
                }
                self.numNodes += 1;

                currentNode = switch (currentNode.layout) {
                    .Compressed => |c| c.nextPtr,
                    .Branching => |b| &(b[0].child),
                };
            }

            if (currentNode.nodeType == .Link) {
                self.numElements += 1;
            }

            currentNode.nodeType = .{ .Key = value };
            return .New;
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
            var node = &self.head;

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
                    .Compressed => |c| c.nextPtr,
                    .Branching => |b| &(b[j].child),
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
            recursiveShow(0, 0, &self.head);
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

            switch (node.nodeType) {
                else => {},
                .Key => |value| {
                    if (@TypeOf(value) != void) {
                        std.debug.warn("={x}", .{value});
                    }
                    numchars += 4;
                },
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
                    recursiveShow(level + 1, lpad, compressed.nextPtr);
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
                        recursiveShow(level + 1, lpad, &br.child);
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
