const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleIf() !void {
    debugPrint("=================== IF\n", .{});
    //If
    const ab = if (false) [_]u8{ 1, 4 } else [_]u8{ 2, 7, 9 }; // as ternary
    debugPrint("=> {any}\n", .{ab});

    if (ab.len < 3) {
        debugPrint("=> should not get called\n", .{});
    } else if (ab.len == 3) {
        debugPrint("=> {d}\n", .{ab.len});
    } else {
        unreachable;
    }

    var cde: ?u32 = null;
    if (cde) |_| {
        unreachable;
    } else {
        debugPrint("=> {?}\n", .{cde});
    }
    cde = 100;
    if (cde) |val| {
        debugPrint("=> {?}\n", .{val});
    } else {
        unreachable;
    }

    // value by reference using pointer capture
    if (cde) |*val| {
        val.* += 10;
    } else {
        unreachable;
    }
    debugPrint("=> {?}\n", .{cde});

    var fgh: anyerror!u32 = 200;
    if (fgh) |val| {
        debugPrint("=> {?}\n", .{val});
    } else |_| {
        unreachable;
    }

    fgh = error.BadValue;
    if (fgh) |_| {
        unreachable;
    } else |err| {
        debugPrint("=> {?}\n", .{err});
    }

    var ijk: anyerror!?u32 = 0;
    if (ijk) |optional_value| {
        debugPrint("=> {?}\n", .{optional_value.?});
    } else |_| {
        unreachable;
    }
}
