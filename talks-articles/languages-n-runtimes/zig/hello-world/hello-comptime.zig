const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleComptime() !void {
    debugPrint("=================== COMPTIME\n", .{});
    //comptime
    debugPrint("=> 3.5 || 2.5 : {?}\n", .{max(f32, 3.5, 2.5)});
    debugPrint("=> 3 || 5 : {?}\n", .{max(i32, 3, 5)});
    debugPrint("=> false || true : {?}\n", .{maxWithBool(bool, false, true)});

    comptime var num = 2; // have to be comptime
    inline for (0..pow_fns.len) |idx| {
        debugPrint("=> 2 pow {?} : {?}\n", .{ pow_fns[idx].power, pow_fns[idx].func(num) });
    }

    var numbers: [10]u32 = undefined;
    var lst = List(u32){
        .items = &numbers,
        .len = numbers.len,
    };
    for (0..lst.len) |idx| {
        var x: u32 = @truncate(idx);
        lst.items[idx] = x * x;
    }
    debugPrint("=> {any}\n", .{lst});
}

fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

fn maxWithBool(comptime T: type, a: T, b: T) T {
    if (T == bool) return a or b;
    return if (a > b) a else b;
}

const pow_fns = [_]PowFn{
    PowFn{ .power = 2, .func = pow2 },
    PowFn{ .power = 3, .func = pow3 },
    PowFn{ .power = 4, .func = pow4 },
};
const PowFn = struct {
    power: u8,
    func: fn (u32) u32,
};
fn pow2(val: u32) u32 {
    return val * val;
}
fn pow3(val: u32) u32 {
    return val * val * val;
}
fn pow4(val: u32) u32 {
    return val * val * val * val;
}

fn List(comptime T: type) type {
    const L = struct {
        items: []T,
        len: usize = 0,
    };
    return L;
}
