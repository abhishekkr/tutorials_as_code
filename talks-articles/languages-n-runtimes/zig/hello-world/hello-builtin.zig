const builtin = @import("builtin");
const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleBuiltin() !void {
    debugPrint("=================== COMPILE VARIABLES\n", .{});
    //builtin: compile variables
    if (builtin.os.tag == .linux)
        debugPrint("OS Tag: {?}\n", .{builtin.os.tag});

    debugPrint("Zig Version: {?}\n", .{builtin.zig_version});
    debugPrint("Zig Backend: {?}\n", .{builtin.zig_backend});
    debugPrint("Output Mode: {?}\n", .{builtin.output_mode});
    debugPrint("Link Mode: {?}\n", .{builtin.link_mode});
    debugPrint("Single Threaded: {?}\n", .{builtin.single_threaded});
    debugPrint("ABI: {?}\n", .{builtin.abi});
    debugPrint("CPU: {?}\n", .{builtin.cpu.arch});
}
