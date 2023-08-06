const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleFor() !void {
    debugPrint("=================== FOR\n", .{});
    //For
    var value: usize = 0;
    // to range over any iterable
    for (0..5) |val| {
        if (val % 2 == 0) {
            value += val;
        }
    }
    assert(value == 6);

    // to have index, alongwith value; can _ to void value if not needed
    var items = [_]usize{ 10, 11, 12, 13, 14 };
    for (items, 0..) |val, idx| {
        if (idx % 2 == 0) {
            value += val;
        }
    }
    assert(value == 42);

    // to range over 2 iterables of same length
    for (9..15, [6]u8{ 9, 10, 11, 12, 13, 14 }) |i, j| {
        if (j == 10) {
            value += i;
            debugPrint("i & j @ {d}\n", .{i});
        }
    }
    assert(value == 52);

    // by reference
    for (&items) |*val| {
        val.* += 1;
        debugPrint("item: {d}\n", .{val.*});
    }

    // for else
    const lst = [_]?u8{ 1, 2, null, 100 };
    var sum: u8 = 0;
    const result = for (lst) |val| {
        if (val != null) {
            sum += val.?;
        }
    } else blk: {
        break :blk sum;
    };
    debugPrint("sum: {d}\n", .{result});

    // labeled for
    outer: for (1..4) |i| {
        for (1..6) |j| {
            debugPrint("outer: {d} | inner: {d}\n", .{ i, j });
            if (j == 2) {
                break :outer;
            }
        }
    }

    // inline for, again similar to as that of while
}
