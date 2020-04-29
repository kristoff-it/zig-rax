const std = @import("std");

fn RaxStack(comptime NodeT: type, comptime StaticSize: usize) type {
    return struct {
        maxItems: usize,
        staticItems: [StaticSize]*NodeT,
        stack: []*NodeT,

        const Self = @This();
        fn init(self: *Self) void {
            self.* = Self{
                .maxItems = StaticSize,
                .staticItems = undefined,
                .stack = &self.staticItems,
            };

            self.stack.len = 0;
        }

        fn deinit(self: *Self, allocator: *std.mem.Allocator) void {
            if (self.stack.ptr != &self.staticItems) {
                allocator.free(self.stack);
            }
        }

        // The self.stack slice has a lenght always corresponding to the
        // number of pointers stored in the backing array. Since we allocate
        // space for those in bulk (by doubling the current maxItems value),
        // when adding new items we also manipulate the slice's `.len` field.
        // This means that we use the slice unsafely in .push and .pop, but in
        // exchange external uses can just use the slice normally and be sure
        // that they won't read undefined values (aka garbage).
        fn push(self: *Self, allocator: *std.mem.Allocator, item: *NodeT) !void {
            if (self.stack.len == self.maxItems) {
                if (self.stack.ptr == &self.staticItems) {
                    self.stack = try allocator.alloc(*NodeT, self.maxItems * 2);
                    std.mem.copy(*NodeT, self.stack, &self.staticItems);
                    self.stack.len = self.maxItems;
                } else {
                    self.stack = try allocator.realloc(self.stack, self.maxItems * 2);
                    self.stack.len = self.maxItems;
                }
                self.maxItems *= 2;
            }
            self.stack.ptr[self.stack.len] = item;
            self.stack.len += 1;
        }

        fn pop(self: *Self) *NodeT {
            if (self.stack.len == 0) @panic("tried to pop from an empty stack");
            self.stack.len -= 1;
            return self.stack.ptr[self.stack.len];
        }

        fn peek(self: *Self) ?*NodeT {
            if (self.stack.len == 0) return null;
            return self.stack.ptr[self.stack.len - 1];
        }
    };
}

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

        fn removeChild(self: *Self, allocator: *std.mem.Allocator, child: *Self) void {
            switch (self.layout) {
                .Compressed => |c| {
                    // TODO: make this debug-only
                    if (c.next != child) {
                        @panic("tried to remove a child that was not present in the parent");
                    }

                    allocator.free(c.chars);
                    self.layout = .{ .Branching = &[0]Self.Branch{} };
                },
                .Branching => |branches| {
                    var childIdx = branches.len;
                    for (branches) |b, i| {
                        if (b.child == child) {
                            childIdx = i;
                            break;
                        }
                    }

                    if (childIdx != branches.len) {
                        std.mem.copy(Self.Branch, branches[childIdx..], branches[(childIdx + 1)..]);
                        self.layout.Branching = allocator.shrink(branches, branches.len - 1);
                    } else {
                        @panic("tried to remove a child that was not present in the parent");
                    }
                },
            }
        }

        fn size(self: Self) usize {
            return switch (self.layout) {
                .Compressed => |c| c.chars.len,
                .Branching => |b| b.len,
            };
        }
    };
}

pub fn Rax(comptime V: type) type {
    const NodeT = comptime RaxNode(V);
    const StackT = comptime RaxStack(NodeT, 32);

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

        pub const OperationResult = union(enum) {
            Nothing,
            Found: V,
        };

        pub fn insert(self: *Self, key: []const u8, value: V) !OperationResult {
            return insertImpl(self, key, value, false);
        }
        pub fn insertOverride(self: *Self, key: []const u8, value: V) !OperationResult {
            return insertImpl(self, key, value, true);
        }

        // TODO: override is not comptime. Should it be?
        fn insertImpl(self: *Self, key: []const u8, value: V, override: bool) !OperationResult {
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
                    return OperationResult{ .Found = oldValue };
                } else {
                    currentNode.key = value;
                    self.numElements += 1;
                    return .Nothing;
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
                const isEmpty = currentNode.size() == 0;

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
            return .Nothing;
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

        /// Removes a key from the tree. The return value will contain
        /// the value corresponding to the removed key, if the key was found.
        pub fn remove(self: *Self, key: []const u8) !OperationResult {
            var treeStack: StackT = undefined;
            treeStack.init();
            defer treeStack.deinit(self.allocator);

            const wr = try self.lowWalkStack(key, &treeStack);
            var i = wr.bytesMatched;
            var currentNode = wr.stopNode;
            const splitPos = wr.splitPos;

            // No match was found
            {
                const middleOfCompressedNode = currentNode.layout == .Compressed and splitPos != 0;
                if (i != key.len or middleOfCompressedNode or currentNode.key == null) {
                    return .Nothing;
                }
            }

            const result = OperationResult{ .Found = currentNode.key.? };

            currentNode.key = null;
            self.numElements -= 1;

            var tryToCompress = false;

            const size = currentNode.size();
            if (size == 0) {
                var child = currentNode;
                while (currentNode != self.head) {
                    child = currentNode;
                    _ = child.deinit(self.allocator, 0);
                    self.numNodes -= 1;

                    currentNode = treeStack.pop();
                    const hasChildren = currentNode.layout == .Branching and currentNode.layout.Branching.len != 1;
                    if (currentNode.key != null or hasChildren) {
                        break;
                    }
                }

                // We now need to remove a reference to the last freed child
                // from the parent node currently pointed to by currentNode.
                // (Original code checked here if child is not null.)
                currentNode.removeChild(self.allocator, child);
                if (currentNode.size() == 1 and currentNode.key == null) {
                    tryToCompress = true;
                }
            } else if (size == 1) {
                tryToCompress = true;
            }

            // TODO: no compression if oom

            if (tryToCompress) {
                var parent: *NodeT = undefined;
                while (true) {
                    parent = treeStack.pop(); // this is where we might be popping null from it
                    const hasChildren = parent.layout == .Branching and parent.layout.Branching.len != 1;
                    if (parent.key != null or hasChildren) {
                        break;
                    }

                    currentNode = parent;
                }

                const startNode = currentNode;

                var compressedSize = currentNode.size();
                var nodes: u32 = 1;

                // TODO: right now we repurpose currentNode even when
                // it might make more sense to realloc another node that already
                // is using more memory. Example:
                //     `-(b) [a] -> "nana" -> []=3
                // would it be worht it to try and remember which is the biggest node
                // we encountered previously when running down the tree and realloc that
                // instead of currentNode unconditionally?
                while (currentNode.size() != 0) {
                    currentNode = switch (currentNode.layout) {
                        .Compressed => |c| c.next,
                        .Branching => |b| b[b.len - 1].child, // TODO: does it have to be the last? why not the first?
                    };

                    const hasChildren = currentNode.layout == .Branching and currentNode.layout.Branching.len != 1;
                    if (currentNode.key != null or hasChildren) {
                        break;
                    }

                    if (compressedSize + currentNode.size() > std.math.maxInt(usize)) {
                        break;
                    }

                    nodes += 1;
                    compressedSize += currentNode.size();
                }

                // This is the last node we visited, which is the first
                // node that will *NOT* be compressed.
                const lastNode = currentNode;

                if (nodes > 1) {
                    // Save startNode's child pointer because we need it
                    // and we're about to clobber the node's layout.
                    // Realloc chars in startNode to make space for
                    // all the extra chars coming from its children.
                    var startNodeCharCount: usize = undefined;
                    switch (startNode.layout) {
                        .Compressed => |*c| {
                            currentNode = c.next;
                            startNodeCharCount = c.chars.len;

                            c.chars = try self.allocator.realloc(c.chars, compressedSize);
                            c.next = lastNode;
                        },
                        .Branching => |b| {
                            const branch = b[b.len - 1];
                            startNodeCharCount = 1;

                            currentNode = branch.child; // TODO: does it have to be the last? why not the first?
                            self.allocator.free(b);
                            startNode.layout = .{
                                .Compressed = .{
                                    .chars = try self.allocator.alloc(u8, compressedSize),
                                    .next = lastNode,
                                },
                            };
                            startNode.layout.Compressed.chars[0] = branch.char;
                        },
                    }

                    // Run down the compressable path again,
                    // but this time we copy over chars & free nodes
                    // as we go.
                    const newChars = startNode.layout.Compressed.chars;
                    while (true) {
                        // Copy chars and move down
                        const oldNode = currentNode;
                        switch (currentNode.layout) {
                            .Compressed => |c| {
                                std.mem.copy(u8, newChars[startNodeCharCount..], c.chars);
                                startNodeCharCount += c.chars.len;
                                currentNode = c.next;
                                self.allocator.free(c.chars);
                            },
                            .Branching => |b| {
                                const branch = b[b.len - 1]; // TODO: does it have to be the last? why not the first?
                                newChars[startNodeCharCount] = branch.char;
                                startNodeCharCount += 1;
                                currentNode = branch.child;
                                self.allocator.free(b);
                            },
                        }
                        self.allocator.destroy(oldNode);
                        self.numNodes -= 1;

                        // We stop once we reach the previously identified lastNode.
                        if (currentNode == lastNode) {
                            break;
                        }
                    }
                }
            }

            return result;
        }

        // TODO: should s be named key? Do we ever search
        // for "partials"?
        // TODO: is returning a struct better or worse than
        //       writing to pointers?
        const WalkResult = struct {
            bytesMatched: usize,
            stopNode: *NodeT,
            splitPos: usize, // In real impl it's u29
        };

        fn lowWalk(self: *Self, s: []const u8) WalkResult {
            return self.lowWalkImlp(s, null) catch unreachable;
        }

        fn lowWalkStack(self: *Self, s: []const u8, treeStack: *StackT) !WalkResult {
            return self.lowWalkImlp(s, treeStack);
        }

        // This function cannot fail if no treeStack is provided.
        fn lowWalkImlp(self: *Self, s: []const u8, treeStack: ?*StackT) !WalkResult {
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

                // If a stack was provided, push the visited
                // node's pointer into it.
                if (treeStack) |ts| try ts.push(self.allocator, node);

                node = switch (node.layout) {
                    .Compressed => |c| c.next,
                    .Branching => |b| b[j].child,
                };

                j = 0;
            }

            return WalkResult{
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
