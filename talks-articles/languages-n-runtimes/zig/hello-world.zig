//! A simple Hello World zig program.
//! to just run: zig run hello-world.zig
//! to build exe: zig build-exe hello-world.zig
//! to create executable at given path: zig build-exe -femit-bin=/tmp/hello hello-world.zig
const std = @import("std");
const math = std.math;
const debugPrint = std.debug.print;
const assert = std.debug.assert;
const stdMem = std.mem; // to compare bytes
const testing = std.testing;
const builtin = @import("builtin");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const world = "Zig Land";
    try stdout.print("Hello, {s}!\n", .{world});
    try sampleValues();
    try sampleStrings();
    if (fooDetect() == true) {
        std.log.info("check no WARN statements were made", .{});
    }
    try sampleVariables();
    try sampleOp();
    try sampleCompositeType();
    debugPrint("done.\n", .{});
}

fn sampleValues() !void {
    debugPrint("VALUES\n", .{});
    // integers
    const two_times_two: i32 = 2 * 2;
    debugPrint("=> {}\n", .{two_times_two});

    // floats
    const two_by_four: f32 = 2.0 / 4.0;
    debugPrint("=> {}\n", .{two_by_four});
    debugPrint("=> {d}\n", .{two_by_four});
    debugPrint("=> {d}\n", .{two_by_four + 0.25});
    debugPrint("=> {d:.3}\n", .{two_by_four});
    const twotwo_by_seven: f64 = 22.0 / 7.0;
    debugPrint("=> {d:.10}\n", .{twotwo_by_seven});

    // boolean
    debugPrint("=> {}\n", .{true and false});
    debugPrint("=> {}\n", .{!false});

    // optional
    var opt_val: ?[]const u8 = null;
    assert(opt_val == null);
    debugPrint("=> {?s} of type: {}\n", .{ opt_val, @TypeOf(opt_val) });
    opt_val = "Z I G";
    assert(opt_val != null);
    debugPrint("=> {?s} of type: {}\n", .{ opt_val, @TypeOf(opt_val) });

    // error union
    var err_count: anyerror!i32 = error.ArgNotFound;
    debugPrint("=> {!} of type: {}\n", .{ err_count, @TypeOf(err_count) });
    err_count = 100;
    debugPrint("=> {!} of type: {}\n", .{ err_count, @TypeOf(err_count) });

    // var with undefined
    var lucky: u8 = undefined;
    lucky = 7;
    debugPrint("=> {d}\n", .{lucky});
}

fn sampleStrings() !void {
    const ehlo = "EHLO WORLD";
    debugPrint("STRINGS\n", .{});
    debugPrint("=> {}\n", .{@TypeOf(ehlo)});
    debugPrint("=> {d}\n", .{ehlo.len});
    debugPrint("=> {c}\n", .{ehlo[1]});
    debugPrint("=> {d}\n", .{ehlo[0]});
    debugPrint("=> {}\n", .{ehlo[4] == '\x20'});

    debugPrint("=> {d}\n", .{'\u{1f4a9}'});
    debugPrint("=> {d}\n", .{'\u{1f4af}'});
    debugPrint("=> {d}\n", .{'💯'});
    debugPrint("=> 0x{x}\n", .{"\xff"[0]});
    debugPrint("=> {u}\n", .{'⚡'});
    debugPrint("=> {s} == EHLO\\x20WORLD | {}\n", .{ ehlo, stdMem.eql(u8, ehlo, "EHLO\x20WORLD") });

    const multi_ehlo =
        \\EHLO
        \\WORLD
    ;
    debugPrint("=> {s}\n", .{multi_ehlo});
}

// Zig Test
// to run just this test use 'zig test hello-world.zig'
test "expect addXY to add X & Y" {
    try testing.expect(addXY(5, 5) == 10);

    _ = S;
}

fn addXY(x: i32, y: i32) i32 {
    return x + y;
}

const S = struct {
    test "demo container.X" {
        try testing.expect(1 == 1);
    }

    const SE = enum {
        V,

        // not run as not referenced
        test "demo container.Y" {
            try testing.expect(1 == 100);
        }
    };
};

test "this will be skipped" {
    testing.expect(1 == 100) catch return error.SkipZigTest;
}

// //current self-hosted compiler doesn't support async; would have to build
// test "async skip test unless nosuspend" {
//     var frame = async fooAsync(1);
//     const result = await frame;
//     try testing.expect(result == 2);
// }
//
// fn fooAsync(num: i32) i32 {
//     suspend {
//         resume @frame();
//     }
//     return num * 2;
// }

test "detect leak" {
    var list = std.ArrayList(u21).init(testing.allocator);
    defer list.deinit(); // to cause Memory Leak; comment this line
    try list.append('A');
    try list.append('B');
    try list.append('K');
    try testing.expect(list.items.len == 3);
}

test "detecting test build" {
    try testing.expect(fooDetect() == true);
}

fn fooDetect() bool {
    if (builtin.is_test == true) {
        std.log.warn("\n[WARN] This is called while in test.\n", .{});
    }
    return true;
}

// Container Var
const Jabber = enum {
    blah,
    @"yada yada yada",
};

fn sampleVariables() !void {
    const yjabber: Jabber = .@"yada yada yada";
    const bjabber: Jabber = .blah;
    debugPrint("{}\n{}\n", .{ yjabber, bjabber });
    debugPrint("{}\n", .{Jabber.@"yada yada yada"});

    const SampleStr = struct {
        var x: i32 = 99;
    };
    SampleStr.x += 1;
    debugPrint("SampleStr.x = {}\n", .{SampleStr.x});

    threadLocalStorage(1);
    sampleComptime(1);
    sampleNumbers();
}

threadlocal var tlx: i32 = 9;
fn threadLocalStorage(n: i32) void {
    std.log.debug("in-fn: tlx = {} + {}", .{ tlx, n });
    tlx += n;
    std.log.debug("in-fn: tlx = {}", .{tlx});
}

test "thread local storage" {
    threadLocalStorage(10);
    const threadA = try std.Thread.spawn(.{}, threadLocalStorage, .{100});
    const threadB = try std.Thread.spawn(.{}, threadLocalStorage, .{1000});
    threadLocalStorage(10000);
    threadA.join();
    threadB.join();
    std.log.debug("threadLocalStorage, tlx: {}", .{tlx});
}

fn sampleComptime(n: i8) void {
    comptime var i: i32 = 1;
    if (i < 10) {
        i += 1;
    }
    debugPrint("comptime | i = {}; but being comptime can't be used in an expression with a variable like (n={})\n", .{ i, n });
}

fn sampleNumbers() void {
    const decimal_int = 101_010;
    const hex_int = 0x18A92;
    const octal_int = 0o305222;
    const binary_int = 0b1_1000_1010_1001_0010;
    // all int kinds can use underscores
    if (decimal_int == hex_int) {
        debugPrint("{} == 0x{x}\n", .{ decimal_int, decimal_int });
    }
    if (octal_int == hex_int) {
        debugPrint("{} == 0o{o}\n", .{ decimal_int, decimal_int });
    }
    if (octal_int == binary_int) {
        debugPrint("{} == 0b{b}\n", .{ decimal_int, decimal_int });
    }

    const float_count = 123.0E+3;
    const other_float = 123000.0;
    const nanosecond = 0.000_000_001;
    const hex_float = 0x4_5678.9ABC;
    if (float_count == other_float) {
        debugPrint("float count: {d} | other float: {d}\n", .{ float_count, other_float });
        debugPrint("nanoSec: {}\nhex was: {}\n", .{ nanosecond, hex_float });
    }

    const nan = math.nan(f32);
    const inf = math.inf(f32);
    const neg_inf = -(math.inf(f32));
    debugPrint("NaN != 1.0 : {}\n", .{nan != 1.0});
    debugPrint("Infinite > 1.0 : {}\n", .{inf > 1.0});
    debugPrint("1.0 < NegInfinite: {}\n", .{(1.0 < neg_inf)});
    debugPrint("1.0 > NegInfinite: {}\n", .{(1.0 > neg_inf)});
}

export fn floater_optimized(x: f64) f64 {
    @setFloatMode(.Optimized); // not required for local call
    const big = @as(f64, 1 << 40);
    return x * big / big;
}

test "big float mode" {
    const x = 0.001;
    std.log.warn("optimized = {}\n", .{floater_optimized(x)});
    std.log.warn("strict = {}\n", .{floater_optimized(x)});
}

fn sampleOp() !void {
    var max_i8: i8 = std.math.maxInt(i8);
    //assert(@as(i8, max_i8) *| 2 == 255);
    max_i8 +|= 10;
    debugPrint("max_i8 + 10 = {}\n", .{max_i8});
    max_i8 *%= 2;
    debugPrint("max_i8 * 2 = {}\n", .{max_i8});

    const shift_left_a = 1 << 3;
    debugPrint("1 << 3 = {}\n", .{shift_left_a});
    const shift_right_b = 16 >> 1;
    debugPrint("16 >> 1 = {}\n", .{shift_right_b});

    var ba: i8 = 0b0100;
    ba &= 0b1100;
    debugPrint("0b0100 & 0b10 = {}\n", .{ba});
    ba |= 0b0010;
    debugPrint("0b0100 & 0b10 = {}\n", .{ba});
    ba ^= 0b0010;
    debugPrint("0b0100 & 0b10 = {}\n", .{ba});
    ba = ~ba;
    debugPrint("0b0100 & 0b10 = {}\n", .{ba});

    error.ArgNotFound catch {
        debugPrint("Catch doesn't always have to capture err.\n", .{});
    };

    const ba_ptr = &ba;
    debugPrint("Dereferencing Pointer: {}\n", .{ba_ptr.*});

    const arr_x = [_]u8{ 1, 3, 5 }; // must be comptime
    const arr_y = [_]u8{ 2, 4 }; // can be different sizes
    var arr_xy = arr_x ++ arr_y; // doesn't need to be const
    arr_xy[0] = 100;
    debugPrint("{any} ++ {any} == {any}\n", .{ arr_x, arr_y, arr_xy });

    const arr_a = [_]u8{ 1, 2, 3 }; // must be comptime
    const arr_b = 2; // can be different sizes
    var arr_ab = arr_a ** arr_b; // doesn't need to be const
    debugPrint("{any} ** {any} == {any}\n", .{ arr_a, arr_b, arr_ab });
    debugPrint("\"+-\" ** 10 == {s}\n", .{("+-" ** 10)});
}

fn sampleCompositeType() !void {
    try sampleArrays();
    try sampleVectors();
}

fn sampleArrays() !void {
    //Arrays
    const str = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    assert(stdMem.eql(u8, &str, "hello"));

    var str_upper: [str.len]u8 = undefined;
    var idx: u8 = 0;
    for (str) |chr| {
        str_upper[idx] = chr - 32;
        idx += 1;
    }

    var numbers: [5]i64 = undefined;
    for (&numbers, 0..numbers.len) |*element, i| {
        element.* = @intCast(i * 100);
    }

    debugPrint("=> {any}\n", .{numbers ++ [_]i64{ 600, 700 }});
    debugPrint("=> {any}\n", .{numbers ** 2});

    // comptime init of array
    const Point = struct {
        x: i32,
        y: i32,
    };
    var fancy_array = init: {
        var initial_value: [10]Point = undefined;
        for (&initial_value, 0..) |*pt, i| {
            pt.* = Point{
                .x = @intCast(i),
                .y = @intCast(i * 2),
            };
        }
        break :init initial_value;
    };
    assert(fancy_array[4].x == 4);
    assert(fancy_array[4].y == 8);

    // sentinel-terminated; null-terminated below by making x=0
    const array = [_:0]u8{ 1, 2, 3, 4 };
    assert(array[4] == 0);
    assert(array.len == 4);
    debugPrint("=> {} | {any}\n", .{ @TypeOf(array), array });
}

fn sampleVectors() !void {
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
