const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleVectors() !void {
    debugPrint("=================== VECTORS\n", .{});
    //Vectors
    const a = @Vector(3, i32){ 1, 10, 100 };
    const b = @Vector(3, i32){ 2, 9, 99 };
    debugPrint("vector.a + vector.b = {any}\n", .{a + b});
    debugPrint("vector.a - vector.b = {any}\n", .{a - b});
    debugPrint("vector.a > vector.b = {any}\n", .{a > b});

    var arx: [5]f16 = [_]f16{ 1.2, 1.3, 2.1, 2.3, 3.1 };
    var vex: @Vector(3, f16) = arx[1..4].*;
    debugPrint("vex: {any}\n", .{vex});

    var slice: []const f16 = &arx;
    var offset: u16 = 1;
    // extract comptime-known length from runtime-known offset
    // by doing it in steps;
    // step.1: slice from offset, step.2: array of comptime length
    debugPrint("xo: {any}\n", .{slice[offset..]});
    var vexo: @Vector(2, f16) = slice[offset..][2..4].*;
    debugPrint("vexo: {any}\n", .{vexo});
}
