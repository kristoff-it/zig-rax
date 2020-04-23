const std = @import("std");
const Rax = @import("./rax.zig").Rax;

pub fn main() !void {
    const MyRax = Rax(void);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();

    _ = try r.insert("banana", {});

    r.show();
}
