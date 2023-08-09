const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub fn sampleMem() !void {
    debugPrint("=================== MEMORY\n", .{});
    //memory

    try sampleHW1();
    try sampleArena();
}

fn sampleHW1() !void {
    var buf: [10]u8 = undefined; // if less than required, would give Overflow
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const allocator = fba.allocator();
    const result = try concat(allocator, "Hello", "World");
    assert(std.mem.eql(u8, "HelloWorld", result));
    debugPrint("=> {s}\n", .{result});
}

fn concat(allocator: Allocator, a: []const u8, b: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, a.len + b.len);
    std.mem.copy(u8, result, a);
    std.mem.copy(u8, result[a.len..], b);
    return result;
}

fn sampleArena() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit(); // frees everything

    const allocator = arena.allocator();
    const ptr = try allocator.create(u8);
    debugPrint("ptr = {*}\n", .{ptr});
}
