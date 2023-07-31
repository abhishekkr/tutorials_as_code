const std = @import("std");
const debugPrint = std.debug.print;
const stdMem = std.mem; // to compare bytes

pub fn sampleStrings() !void {
    debugPrint("=================== STRINGS\n", .{});
    const ehlo = "EHLO WORLD";
    debugPrint("STRINGS\n", .{});
    debugPrint("=> {}\n", .{@TypeOf(ehlo)});
    debugPrint("=> {d}\n", .{ehlo.len});
    debugPrint("=> {c}\n", .{ehlo[1]});
    debugPrint("=> {d}\n", .{ehlo[0]});
    debugPrint("=> {}\n", .{ehlo[4] == '\x20'});

    debugPrint("=> {d}\n", .{'\u{1f4a9}'});
    debugPrint("=> {d}\n", .{'\u{1f4af}'});
    debugPrint("=> {d}\n", .{'💯'});
    debugPrint("=> 0x{x}\n", .{"\xff"[0]});
    debugPrint("=> {u}\n", .{'⚡'});
    debugPrint("=> {s} == EHLO\\x20WORLD | {}\n", .{ ehlo, stdMem.eql(u8, ehlo, "EHLO\x20WORLD") });

    const multi_ehlo =
        \\EHLO
        \\WORLD
    ;
    debugPrint("=> {s}\n", .{multi_ehlo});
}
