const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleOp() !void {
    debugPrint("=================== OPERATORS\n", .{});
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
