const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleZeroBit() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    var allocator = gpa.allocator();
    var x: void = {};

    var map = std.AutoHashMap(i32, void).init(allocator);
    defer map.deinit();

    try map.put(1, x);
    try map.put(2, {});

    debugPrint("map contains 1: {?}\n", .{map.contains(1)});
    debugPrint("map contains 2: {?}\n", .{map.contains(2)});
    debugPrint("map contains 3: {?}\n", .{map.contains(3)});

    var rem2 = map.remove(2);
    debugPrint("rem2: {?} | map contains 2: {?}\n", .{ rem2, map.contains(2) });
}
