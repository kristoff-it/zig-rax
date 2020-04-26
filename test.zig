const std = @import("std");
const Rax = @import("./rax.zig").Rax;

pub fn main() !void {
    const MyRax = Rax(i64);
    var r = try MyRax.init(std.testing.allocator);
    // defer r.deinit();

    const r1 = try r.insert("annibale", 1);
    std.debug.warn("r1={} nodes={} elems={}\n", .{ r1, r.numNodes, r.numElements });
    const r2 = try r.insert("anni", 2);
    std.debug.warn("r2={} nodes={} elems={}\n", .{ r2, r.numNodes, r.numElements });

    _ = try r.insert("banana", 3);
    _ = try r.insert("anna", 4);
    _ = try r.insert("peach", 5);
    _ = try r.insert("pear", 6);
    _ = try r.insert("watermelon", 7);
    _ = try r.insert("strawberry", 8);
    _ = try r.insert("straw", 9);
    _ = try r.insert("berry", 10);
    _ = try r.insert("a", 11);
    _ = try r.insert("b", 12);
    _ = try r.insert("p", 13);
    r.show();
}
