const std = @import("std");

pub fn RaxIterator(comptime RaxT: type, comptime StaticSize: usize) type {
    return struct {
        tree: *RaxT,
        data: ?RaxT.ValueT,
        key: []u8,
        keyMaxLen: usize,
        keyStaticBuf: [StaticSize]u8,
        allocator: *std.mem.Allocator,
        currentNode: *RaxT.NodeT,
        stack: RaxT.StackT,
        nodeFn: ?fn (node: *RaxT.NodeT) void,
        justSeeked: bool,
        EOF: bool,
        safe: bool,

        const Self = @This();
        pub fn init(self: *Self, tree: *RaxT) void {
            self.* = .{
                .tree = tree,
                .data = null,
                .key = &self.keyStaticBuf,
                .keyMaxLen = StaticSize,
                .keyStaticBuf = undefined,
                .allocator = tree.allocator,
                .currentNode = undefined,
                .stack = undefined,
                .nodeFn = null,
                .justSeeked = undefined,
                .EOF = true, // unseeked iterators return eof
                .safe = undefined,
            };
            self.key.len = 0; // ignore unset bytes in the static buffer
            self.stack.init();
        }
        pub fn deinit(self: *Self) void {
            self.stack.deinit(self.allocator);
            if (self.key) |k| {
                if (k.ptr != &self.keyStaticBuf) {
                    self.allocator.free(k);
                }
            }
        }
        pub const SeekOp = enum {
            First,
            Lt,
            Lte,
            Eq,
            Gte,
            Gt,
            Last,
        };

        pub fn seek(self: *Self, op: SeekOp, elem: []const u8) !void {

            // Resetting state
            self.stack.stack.len = 0;
            self.justSeeked = true;
            self.EOF = false;
            self.key.len = 0;
            // TODO: we're leaving node to undef. bad idea?

            if (self.tree.numElements == 0) {
                self.EOF = true;
                return;
            }

            // Handle first & last
            switch (op) {
                .First => {
                    return self.seek(.Gte, &[0]u8{});
                },
                .Last => {
                    self.currentNode = self.tree.head;
                    try self.seekGreatest();
                    self.data = self.currentNode.key.?;
                    return;
                },
                else => {},
            }

            const wr = try self.tree.lowWalkStack(elem, &self.stack);
            self.currentNode = wr.stopNode;
            const splitPos = wr.splitPos;
            var i = wr.bytesMatched;

            const eqCase = op == .Eq or op == .Lte or op == .Gte;
            const ltCase = op == .Lt or op == .Lte;
            const gtCase = op == .Gt or op == .Gte;
            const fullMatch = i == elem.len;
            const noMidCompressed = self.currentNode.layout != .Compressed or splitPos == 0;
            if (eqCase and fullMatch and noMidCompressed and
                self.currentNode.key != null)
            {
                try self.addChars(elem);
                self.data = self.currentNode.key.?;
                return;
            } else if (ltCase or gtCase) {
                // TODO: I strongly suspect this code is unnecessary, as in
                //       it's doing the right thing (copying in self.key all that we
                //       found until now) but it would be much more efficient to
                //       just copy elem[..i] and then start from the last node.
                //       We did match up until i (and the last node in the stack),
                //       did we not?
                {
                    try self.stack.push(self.allocator, self.currentNode);
                    var j: usize = 1;
                    while (j < self.stack.stack.len) : (j += 1) {
                        const parent = self.stack.stack[j - 1];
                        const child = self.stack.stack[j];

                        switch (parent.layout) {
                            .Compressed => |c| {
                                try self.addChars(c.chars);
                            },
                            .Branching => |branches| {
                                for (branches) |b| {
                                    if (b.child == child) {
                                        try self.addChars(@as(*const [1]u8, &b.char)); // this needs to be a [1]u8
                                        break;
                                    }
                                }
                            },
                        }
                    }
                    const x = self.stack.pop();
                    std.debug.warn("3. pop {*}\n", .{x});
                }

                if (i != elem.len and self.currentNode.layout != .Compressed) {
                    // Mismatch in branching node
                    try self.addChars(elem[i..(i + 1)]);
                    self.justSeeked = false;
                    if (ltCase) try self.prevStep(true);
                    if (gtCase) try self.nextStep(true);
                    self.justSeeked = true;
                } else if (i != elem.len and self.currentNode.layout == .Compressed) {
                    const cData = self.currentNode.layout.Compressed;
                    const nodeChar = cData.chars[splitPos];
                    const keyChar = elem[i];
                    self.justSeeked = false;
                    if (gtCase) {
                        if (nodeChar > keyChar) {
                            try self.nextStep(false);
                        } else {
                            try self.addChars(cData.chars);
                            try self.nextStep(true);
                        }
                    }
                    if (ltCase) {
                        if (nodeChar < keyChar) {
                            try self.seekGreatest();
                            self.data = self.currentNode.key.?;
                            // I think this will panic in Zig, while the C code would
                            // consider it a noop if EOF was set by seekGreatest()
                        } else {
                            try self.addChars(cData.chars);
                            try self.prevStep(true);
                        }
                    }
                    self.justSeeked = true;
                } else {
                    self.justSeeked = false;
                    if (self.currentNode.layout == .Compressed and self.currentNode.key != null and splitPos != 0 and ltCase) {
                        self.data = self.currentNode.key.?;
                        // Safe because we asserted this as part of the if contiditons
                    } else {
                        if (ltCase) try self.prevStep(false);
                        if (gtCase) try self.nextStep(false);
                    }
                    self.justSeeked = true;
                }
            } else {
                // If we are here just eq was set but no match was found.
                self.EOF = true;
            }
        }
        pub const OperationResult = union(enum) {
            EOF,
            Found: RaxT.ValueT,
        };

        pub fn next(self: *Self) !OperationResult {
            try self.nextStep(false);
            if (self.EOF) {
                return .EOF;
            } else {
                return OperationResult{ .Found = self.data.? };
            }
        }
        pub fn prev() void {}
        pub fn randomWalk() void {}
        pub fn compare() void {}

        fn addChars(self: *Self, s: []const u8) !void {
            const oldLen = self.key.len;
            if (oldLen + s.len > self.keyMaxLen) {
                const newMax = (oldLen + s.len) * 2;
                if (self.key.ptr == &self.keyStaticBuf) {
                    // still using the static buf
                    self.key = try self.allocator.alloc(u8, newMax);
                    std.mem.copy(u8, self.key, &self.keyStaticBuf);
                } else {
                    // we were already heap memory
                    self.key = try self.allocator.realloc(self.key, newMax);
                }
                self.key.len = oldLen;
                self.keyMaxLen = newMax;
            }

            // Test overlapping assumption. If wrong, need to add copyBackwards
            {
                const destPtr = @ptrToInt(self.key.ptr);
                const srcPtr = @ptrToInt(s.ptr);
                if (destPtr > srcPtr and
                    (srcPtr + s.len) > destPtr)
                {
                    @panic("how is s.ptr overlapping this way with dest??");
                }
            }

            std.mem.copy(u8, self.key.ptr[oldLen..(oldLen + s.len)], s);
            self.key.len = oldLen + s.len;
        }

        fn delChars(self: *Self, n: usize) void {
            self.key.len -= n;
        }

        fn seekGreatest(self: *Self) !void {
            while (self.currentNode.size() > 0) {
                const oldNode = self.currentNode;
                switch (self.currentNode.layout) {
                    .Compressed => |c| {
                        try self.addChars(c.chars);
                        self.currentNode = c.next;
                    },
                    .Branching => |b| {
                        const lastBranch = b[b.len - 1];
                        try self.addChars(@as(*const [1]u8, &lastBranch.char));
                        self.currentNode = lastBranch.child;
                    },
                }
                // We only push after addChars succeded. (might be not important)
                try self.stack.push(self.allocator, self.currentNode);
            }
        }
        fn nextStep(self: *Self, noupArg: bool) !void {
            if (self.EOF) return;
            if (self.justSeeked) {
                self.justSeeked = false;
                return;
            }

            var noup = noupArg;
            const origLen = self.key.len;
            const origStackLen = self.stack.stack.len;
            const origNode = self.currentNode;

            while (true) {
                if (!noup and self.currentNode.size() > 0) {
                    try self.stack.push(self.allocator, self.currentNode);

                    // Move down
                    switch (self.currentNode.layout) {
                        .Compressed => |c| {
                            try self.addChars(c.chars);
                            self.currentNode = c.next;
                        },
                        .Branching => |b| {
                            const firstBranch = b[0];
                            try self.addChars(@as(*const [1]u8, &firstBranch.char));
                            self.currentNode = firstBranch.child;
                        },
                    }

                    // Call the provided fn, if any.
                    if (self.nodeFn) |f| f(self.currentNode);

                    // Return if we found a key.
                    if (self.currentNode.key) |k| {
                        self.data = k;
                        return;
                    }
                } else {
                    while (true) {
                        const oldNoup = noup;

                        // Stop when reaching head
                        if (!noup and self.currentNode == self.tree.head) {
                            self.EOF = true;
                            self.stack.stack.len = origStackLen;
                            self.key.len = origLen;
                            self.currentNode = origNode;

                            return;
                        }

                        // TODO: I don't get this
                        const prevChildChar = self.key[self.key.len - 1];
                        if (!noup) {
                            const old = self.currentNode;

                            self.currentNode = self.stack.pop();
                        } else {
                            noup = false;
                        }

                        // Delete chars that were discarded by going up.
                        const toDel = switch (self.currentNode.layout) {
                            .Compressed => |c| c.chars.len,
                            .Branching => 1,
                        };
                        self.delChars(toDel);

                        // Try visiting the next child if there was at least
                        // an additional child
                        if (self.currentNode.layout != .Compressed and
                            self.currentNode.layout.Branching.len >
                            (if (oldNoup) @as(usize, 0) else 1))
                        {
                            const branches = self.currentNode.layout.Branching;

                            // Find the first child > prev
                            var i: usize = 0;
                            while (i < branches.len) : (i += 1) {
                                if (branches[i].char > prevChildChar) break;
                            }

                            if (i != branches.len) {
                                try self.addChars(@as(*const [1]u8, &branches[i].char));
                                try self.stack.push(self.allocator, self.currentNode);
                                self.currentNode = branches[i].child;

                                // Call the provided fn, if any.
                                if (self.nodeFn) |f| f(self.currentNode);

                                // Return if we found a key.
                                if (self.currentNode.key) |k| {
                                    self.data = k;
                                    return;
                                }

                                // Break the inner loop
                                break;
                            }
                        }
                    }
                }
            }
        }
        fn prevStep(self: *Self, noupArg: bool) !void {
            if (self.EOF) return;
            if (self.justSeeked) {
                self.justSeeked = false;
                return;
            }

            var noup = noupArg;
            const origLen = self.key.len;
            const origStackLen = self.stack.stack.len;
            const origNode = self.currentNode;

            while (true) {
                const oldNoup = noup;

                // Stop when reaching head
                if (!noup and self.currentNode == self.tree.head) {
                    self.EOF = true;
                    self.stack.stack.len = origStackLen;
                    self.key.len = origLen;
                    self.currentNode = origNode;
                    return;
                }

                // TODO: I don't get this
                const prevChildChar = self.key[self.key.len - 1];
                if (!noup) {
                    self.currentNode = self.stack.pop();
                    std.debug.warn("2. pop {*}\n", .{self.currentNode});
                } else {
                    noup = false;
                }

                // Delete chars that were discarded by going up.
                const toDel = switch (self.currentNode.layout) {
                    .Compressed => |c| c.chars.len,
                    .Branching => 1,
                };
                self.delChars(toDel);

                // Try visiting the next child if there was at least
                // an additional child
                if (self.currentNode.layout != .Compressed and
                    self.currentNode.layout.Branching.len >
                    (if (oldNoup) @as(usize, 0) else 1))
                {
                    const branches = self.currentNode.layout.Branching;

                    // Find the first child > prev
                    var i: usize = branches.len - 1;
                    while (i > 0) : (i -= 1) {
                        if (branches[i].char > prevChildChar) break;
                    }

                    // If i == 0, then we didn't check the condition in the
                    // above while loop. Need to check now.
                    if (i != 0 or branches[i].char > prevChildChar) {
                        try self.addChars(@as(*const [1]u8, &branches[i].char));
                        std.debug.warn("3. pushing {*}\n", .{self.currentNode});
                        try self.stack.push(self.allocator, branches[i].child);
                        self.currentNode = branches[i].child;
                        try self.seekGreatest();
                    }
                }

                // Return if we found a key.
                if (self.currentNode.key) |k| {
                    self.data = k;
                    return;
                }
            }
        }
    };
}
