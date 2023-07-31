const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const builtin = @import("builtin");

// Zig Test
// to run just this test use 'zig test hello-world.zig'
test "expect addXY to add X & Y" {
    std.log.warn("\n[WARN] Logging Test Warning. <=====\n", .{});
    try testing.expect(addXY(5, 5) == 10);

    _ = S;
}

fn addXY(x: i32, y: i32) i32 {
    return x + y;
}

pub const S = struct {
    test "demo container.X" {
        try testing.expect(1 == 1);
    }

    const SE = enum {
        V,

        // not run as not referenced
        test "demo container.Y" {
            try testing.expect(1 == 100);
        }
    };
};

test "this will be skipped" {
    testing.expect(1 == 100) catch return error.SkipZigTest;
}

// //current self-hosted compiler doesn't support async; would have to build
// test "async skip test unless nosuspend" {
//     var frame = async fooAsync(1);
//     const result = await frame;
//     try testing.expect(result == 2);
// }
//
// fn fooAsync(num: i32) i32 {
//     suspend {
//         resume @frame();
//     }
//     return num * 2;
// }

test "detect leak" {
    var list = std.ArrayList(u21).init(testing.allocator);
    defer list.deinit(); // to cause Memory Leak; comment this line
    try list.append('A');
    try list.append('B');
    try list.append('K');
    try testing.expect(list.items.len == 3);
}

test "detecting test build" {
    try testing.expect(fooDetect() == true);
}

pub fn fooDetect() bool {
    if (builtin.is_test == true) {
        std.log.warn("\n[WARN] This is called while in test. <=====\n", .{});
    }
    return true;
}
