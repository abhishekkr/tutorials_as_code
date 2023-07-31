const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;
const stdMem = std.mem; // to compare bytes

pub fn sampleArrays() !void {
    debugPrint("=================== ARRAYS\n", .{});
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
