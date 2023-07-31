const std = @import("std");
const debugPrint = std.debug.print;

// Container Var
const Jabber = enum {
    blah,
    @"yada yada yada",
};

pub fn sampleVariables() !void {
    debugPrint("=================== VARIABLES\n", .{});
    const yjabber: Jabber = .@"yada yada yada";
    const bjabber: Jabber = .blah;
    debugPrint("{}\n{}\n", .{ yjabber, bjabber });
    debugPrint("{}\n", .{Jabber.@"yada yada yada"});

    const SampleStr = struct {
        var x: i32 = 99;
    };
    SampleStr.x += 1;
    debugPrint("SampleStr.x = {}\n", .{SampleStr.x});

    threadLocalStorage(1);
    sampleComptime(1);
}

threadlocal var tlx: i32 = 9;
fn threadLocalStorage(n: i32) void {
    std.log.debug("in-fn: tlx = {} + {}", .{ tlx, n });
    tlx += n;
    std.log.debug("in-fn: tlx = {}", .{tlx});
}

test "thread local storage" {
    threadLocalStorage(10);
    const threadA = try std.Thread.spawn(.{}, threadLocalStorage, .{100});
    const threadB = try std.Thread.spawn(.{}, threadLocalStorage, .{1000});
    threadLocalStorage(10000);
    threadA.join();
    threadB.join();
    std.log.debug("threadLocalStorage, tlx: {}", .{tlx});
}

fn sampleComptime(n: i8) void {
    comptime var i: i32 = 1;
    if (i < 10) {
        i += 1;
    }
    debugPrint("comptime | i = {}; but being comptime can't be used in an expression with a variable like (n={})\n", .{ i, n });
}
