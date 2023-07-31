const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleSlice() !void {
    debugPrint("=================== SLICES\n", .{});
    var arry = [_]i32{ 4, 3, 2, 1, 0 };
    var length: usize = 1;
    var slyc = arry[length..arry.len];
    slyc[2] += 100; // also updates the value at arry
    debugPrint("{any} | {any}\n", .{ slyc, arry });
    slyc.ptr += 1; // leaves slyc in bad state as len remains unchanged
    debugPrint("{any} | {any}\n", .{ slyc, arry });
    slyc.len += 1; // directly updateable
    debugPrint("{any} | {any}\n", .{ slyc, arry });

    // zig has no concepts of strings,
    // they are const pointers to null-terminated u8 arrays
    const p1: []const u8 = "HTTP";
    const p2: []const u8 = "1.1";
    var p1and2: [64]u8 = undefined;
    // convert array into slice
    var start: usize = 0;
    const p1and2_slice = p1and2[start..];
    // string concat example
    const http1dot1 = try std.fmt.bufPrint(p1and2_slice, "{s}/{s}", .{ p1, p2 });
    debugPrint("{s} with {s} => {s}\n", .{ p1, p2, http1dot1 });
    testSampleNullTermSlice();
}

fn testSampleNullTermSlice() void {
    var arry = [_]u8{ 3, 2, 1, 0, 3, 2, 1, 0 };
    const slyc = arry[0..3 :0];

    debugPrint("=> {}\n", .{@TypeOf(slyc)});
    debugPrint("=> {}\n", .{slyc.len});
}
