const std = @import("std");

pub fn RaxNode(comptime V: type) type {
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
        pub const Branch = struct {
            char: u8,
            child: *Self,
        };

        pub fn initCompressedNode(allocator: *std.mem.Allocator, chars: []const u8, next: *Self) !*Self {
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

        pub fn initBranchNode(comptime N: comptime_int, allocator: *std.mem.Allocator, chars: [N]u8, children: [N]*Self) !*Self {
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

        pub fn initEmpty(allocator: *std.mem.Allocator) !*Self {
            var result = try allocator.create(Self);
            result.* = Self{
                .key = null,
                .layout = .{ .Branching = &[0]Branch{} },
            };
            return result;
        }

        pub fn deinit(self: *Self, allocator: *std.mem.Allocator, deletedNodes: u64) u64 {
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

        pub fn makeCompressed(self: *Self, allocator: *std.mem.Allocator, s: []const u8) !void {
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

        pub fn removeChild(self: *Self, allocator: *std.mem.Allocator, child: *Self) void {
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

        pub fn size(self: Self) usize {
            return switch (self.layout) {
                .Compressed => |c| c.chars.len,
                .Branching => |b| b.len,
            };
        }
    };
}
