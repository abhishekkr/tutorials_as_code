const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleOptional() !void {
    debugPrint("=================== OPTIONALS\n", .{});
    //Optional
    const opt_a: ?u8 = 10;
    var opt_b: ?u8 = 10;
    if (opt_a) |a| {
        if (opt_b) |b| {
            opt_b = a + b;
        }
    }
    debugPrint("=> opt_b {?}\n", .{opt_b});
}
