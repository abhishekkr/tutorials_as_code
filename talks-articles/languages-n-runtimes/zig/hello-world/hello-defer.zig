const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleDefer() !void {
    debugPrint("=================== DEFER\n", .{});
    //Defer
    debugPrint("=> {any} should be 10\n", .{deferX()});
    deferY() catch {};
}

fn deferX() !usize {
    var x: usize = 1;
    {
        defer debugPrint("=> 1\n", .{});
        defer x = 10;
        x = 5;
    }
    defer {
        defer debugPrint("=> 2\n", .{});
    }
    assert(x == 10);
    defer debugPrint("=> 3\n", .{});
    defer debugPrint("=> 4\n", .{});
    defer x = 2;
    return x;
}

fn deferY() !void {
    errdefer |err| {
        debugPrint("=> err: {s}\n", .{@errorName(err)});
    }
    return error.DeferError;
}
