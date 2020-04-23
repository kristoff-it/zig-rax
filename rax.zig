const std = @import("std");

fn RaxNode(comptime V: type) type {
    return struct {
        const Self = @This();

        nodeType: union(enum) {
            isKey: V,
            isNull,
            isLink,
        },
        isCompressed: bool,
        chars: []u8,
        children: []*Self,

        fn init(allocator: *std.mem.Allocator, children: usize) !*Self {
            const result = try allocator.create(Self);
            errdefer allocator.destroy(result);

            const charsSlice = try allocator.alloc(u8, children);
            errdefer allocator.free(charsSlice);

            const childrenSlice = try allocator.alloc(*Self, children);
            errdefer allocator.free(childrenSlice);

            result.* = Self{
                .nodeType = .isLink,
                .isCompressed = false,
                .chars = charsSlice,
                .children = childrenSlice,
            };

            return result;
        }

        fn makeCompressed(self: *Self, allocator: *std.mem.Allocator, s: []const u8) !void {
            if (self.isCompressed or self.chars.len != 0 or self.children.len != 0) {
                @panic("tried to call makeCompressed on an unsuitable node");
            }

            var child = try Self.init(allocator, 0);
            errdefer child.deinit(allocator);

            const charsSlice = try allocator.alloc(u8, s.len);
            errdefer allocator.free(charsSlice);

            std.mem.copy(u8, charsSlice, s);

            const childrenSlice = try allocator.alloc(*Self, 1);
            errdefer allocator.free(childrenSlice);
            childrenSlice[0] = child;

            self.isCompressed = true;
            self.chars = charsSlice;
            self.children = childrenSlice;
        }

        fn deinit(self: Self, allocator: *std.mem.Allocator) void {
            allocator.free(self.chars);
            allocator.free(self.children);
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
                .head = try NodeT.init(allocator, 0),
                .numNodes = 0,
                .numElements = 0,
            };
        }

        pub fn deinit(self: Self) void {
            self.allocator.destroy(self.head);
        }

        pub fn insert(self: *Self, key: []const u8, value: V) !?V {
            return insertImpl(self, key, value, false);
        }
        pub fn insertOverride(self: *Self, key: []const u8, value: V) !?V {
            return insertImpl(self, key, value, true);
        }

        // TODO: override is not comptime. Should it be?
        fn insertImpl(self: *Self, key: []const u8, value: V, override: bool) !?V {
            var wk = self.lowWalk(key);

            var i = wk.bytesMatched;
            var currentNode = wk.stopNode;
            var parentLink = wk.pLink;

            while (i < key.len) {
                if (currentNode.chars.len == 0 and key.len - i > 1) {
                    var comprSize = key.len - i;

                    // this should be maxsize of u29 in the
                    // real implementation.
                    if (comprSize > std.math.maxInt(usize)) {
                        comprSize = std.math.maxInt(usize);
                    }

                    // TODO: error handling procedure
                    try currentNode.makeCompressed(self.allocator, key[i..]);

                    // parentLink.* = currentNode;
                    // parentLink = &currentNode.children[0];

                    i += comprSize;
                } else {
                    // TODO
                    @panic("TODO");
                }
                self.numNodes += 1;
                currentNode = currentNode.children[0]; // TODO: review when implementing the else branch
            }

            switch (currentNode.nodeType) {
                .isKey => {},
                else => self.numElements += 1,
            }

            currentNode.nodeType = .{ .isKey = value };
            return null;
        }

        // TODO: should s be named key? Do we ever search
        // for "partials"?
        // TODO: is returning a struct better or worse than
        //       writing to pointers?
        const walkResult = struct {
            bytesMatched: usize,
            stopNode: *NodeT,
            pLink: **NodeT,
            splitPos: usize, // In real impl it's u29
        };

        fn lowWalk(self: *Self, s: []const u8) walkResult {
            var node = self.head;
            var parentLink: **NodeT = &self.head;

            var i: usize = 0;
            var j: usize = 0;
            while (node.chars.len > 0 and i < s.len) {
                if (node.isCompressed) {
                    j = 0;
                    while (j < node.chars.len and i < s.len) {
                        if (node.chars[j] != s[i]) break;
                        j += 1;
                        i += 1;
                    }
                    if (j != node.chars.len) break;
                } else {
                    // rax does a linear scan, so we do too.
                    while (j < node.chars.len and i < s.len) : (j += 1) {
                        if (node.chars[j] == s[i]) break;
                    }
                    if (j == node.chars.len) break;
                    i += 1;
                }

                if (node.isCompressed) j = 0;
                node = node.children[j];
                parentLink = &node.children[j];
                j = 0;
            }

            return walkResult{
                .bytesMatched = i,
                .stopNode = node,
                .pLink = parentLink,
                .splitPos = j,
            };
        }

        pub fn show(self: Self) void {
            recursiveShow(0, 0, self.head);
            std.debug.warn("\n", .{});
        }

        fn recursiveShow(level: usize, lp: usize, node: *NodeT) void {
            var lpad = lp;
            const start: u8 = if (node.isCompressed) '"' else '[';
            const end: u8 = if (node.isCompressed) '"' else ']';

            std.debug.warn("{c}{}{c}", .{ start, node.chars, end });
            var numchars = 2 + node.chars.len;

            switch (node.nodeType) {
                else => {},
                .isKey => |value| {
                    if (@TypeOf(value) != void) {
                        std.debug.warn("={x}", .{value});
                    }
                    numchars += 4;
                },
            }

            if (level > 0) {
                lpad += if (node.children.len > 1) @as(usize, 7) else @as(usize, 4);
                if (node.children.len == 1) {
                    lpad += numchars;
                }
            }

            for (node.children) |ch, idx| {
                if (node.children.len > 1) {
                    std.debug.warn("\n", .{});
                    var i: usize = 0;
                    while (i < lpad) : (i += 1) std.debug.warn(" ", .{});
                    std.debug.warn(" `-({}) ", .{node.chars[idx]});
                } else {
                    std.debug.warn(" -> ", .{});
                }
                recursiveShow(level + 1, lpad, ch);
            }
        }
    };
}

test "works" {
    const MyRax = Rax(void);
    const r = try MyRax.init(std.testing.allocator);
    defer r.deinit();
}

test "try simple insert" {
    const MyRax = Rax(void);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();

    _ = try r.insert("banana", {});
}
