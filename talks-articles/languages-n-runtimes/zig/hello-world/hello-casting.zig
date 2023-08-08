const std = @import("std");
const debugPrint = std.debug.print;

const Foo = struct { data: u8 };

pub fn sampleCasting() !void {
    debugPrint("=================== CASTING\n", .{});
    //Casting
    const a1: u8 = 10;
    var a: u32 = a1; // int widening; allowed on floats as well
    var b = @as(u64, a);
    _ = b;

    var str_a: []const u8 = "HELLO";
    // constant pointers to arrays to a slice
    var str_b: []const u8 = &[5]u8{ 'H', 'E', 'L', 'L', 79 };
    debugPrint("{s} == {s} | {?}\n", .{ str_a, str_b, std.mem.eql(u8, str_a, str_b) });

    // array length becomes slice length
    var buf: [5]u8 = "HELLO".*;
    const x: []u8 = &buf;
    debugPrint("{s} == HELLO | {?}\n", .{ x, std.mem.eql(u8, x, str_b) });

    const abc: anyerror!u8 = 10;
    if ((try abc) == 10) {
        debugPrint("coerce to error union\n", .{});
    }
}
