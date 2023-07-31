const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleValues() !void {
    debugPrint("=================== VALUES\n", .{});
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
