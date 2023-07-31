const std = @import("std");
const debugPrint = std.debug.print;

pub fn samplePointers() !void {
    debugPrint("=================== POINTERS\n", .{});
    const num: i32 = 101;
    const num_ptr = &num;
    debugPrint("{} == {} | {}\n", .{ num, num_ptr.*, @TypeOf(num_ptr) });

    // sentinel-terminated
    var x_ptr = "h-e-l-l-o";
    debugPrint("{any} should be '*const [9:0]u8'\n", .{@TypeOf(x_ptr)});

    var x5: i32 = 1;
    var x32: i32 = 1234;
    var x128: i128 = 1234;
    const align_of_i5 = @alignOf(@TypeOf(x5));
    const align_of_i32 = @alignOf(@TypeOf(x32));
    const align_of_i128 = @alignOf(@TypeOf(x128));
    debugPrint("{} {} {}\n", .{ align_of_i5, align_of_i32, align_of_i128 });
}
