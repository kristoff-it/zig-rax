const std = @import("std");
const Rax = @import("./rax.zig").Rax;

pub fn main() !void {
    const MyRax = Rax(i64);
    var r = try MyRax.init(std.testing.allocator);
    defer r.deinit();

    const r1 = try r.insert("annibale", 1);
    std.debug.warn("r1={} nodes={}\n", .{ r1, r.numNodes });
    const r2 = try r.insert("annientare", 2);
    std.debug.warn("r2={} nodes={}\n", .{ r2, r.numNodes });

    r.show();
}
