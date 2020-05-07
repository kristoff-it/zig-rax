const std = @import("std");

pub fn RaxStack(comptime NodeT: type, comptime StaticSize: usize) type {
    return struct {
        maxItems: usize,
        staticItems: [StaticSize]*NodeT,
        stack: []*NodeT,

        const Self = @This();
        pub fn init(self: *Self) void {
            self.* = Self{
                .maxItems = StaticSize,
                .staticItems = undefined,
                .stack = &self.staticItems,
            };

            self.stack.len = 0;
        }

        pub fn deinit(self: *Self, allocator: *std.mem.Allocator) void {
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
        pub fn push(self: *Self, allocator: *std.mem.Allocator, item: *NodeT) !void {
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

        pub fn pop(self: *Self) *NodeT {
            if (self.stack.len == 0) @panic("tried to pop from an empty stack");
            self.stack.len -= 1;
            return self.stack.ptr[self.stack.len];
        }

        pub fn peek(self: *Self) ?*NodeT {
            if (self.stack.len == 0) return null;
            return self.stack.ptr[self.stack.len - 1];
        }
    };
}
