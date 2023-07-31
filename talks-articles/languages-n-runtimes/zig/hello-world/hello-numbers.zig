const std = @import("std");
const math = std.math;
const debugPrint = std.debug.print;

pub fn sampleNumbers() !void {
    debugPrint("=================== INTS, FLOATS\n", .{});
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
