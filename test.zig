const std = @import("std");
const Rax = @import("./rax.zig").Rax;

pub fn main() !void {
    const MyRax = Rax(i64);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();

    const r1 = try r.insert("banana", 1);
    std.debug.warn("r1={}\n", .{r1});
    const r2 = try r.insertOverride("banana", 2);
    std.debug.warn("r2={}\n", .{r2});
    r.show();
}
