const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleTuples() !void {
    debugPrint("=================== TUPLES\n", .{});
    //Tuples

    const tplx = .{
        @as(u1, 1),
        @as(f32, 1.3),
        true,
        "yada",
    };
    assert(tplx[0] == 1);
    assert(tplx.@"1" == 1.3);
    assert(tplx.@"2");
    assert(tplx[3][0] == 'y');

    const tply = .{
        @as(u1, 1),
    } ++ .{false} ** 2;
    inline for (tply, 0..) |val, idx| {
        debugPrint("tply[{d}] => {?}\n", .{ idx, val });
    }
}
