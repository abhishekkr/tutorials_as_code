const std = @import("std");
const debugPrint = std.debug.print;

const Id = union { int: i64, str: []const u8 };

pub fn sampleUnions() !void {
    debugPrint("=================== UNIONS\n", .{});
    //Union
    var id = Id{ .str = "0abd123" };
    debugPrint("id: {?} => {s}\n", .{ id, id.str });
    // to activate another field, reassign union
    id = Id{ .int = 101 };
    debugPrint("id: {?} => {d}\n", .{ id, id.int });

    // to switch, need tagged union
    const UTag = enum { ok, err };
    const Uni = union(UTag) { ok: u8, err: void };
    var uvar = Uni{ .ok = 100 };
    switch (uvar) {
        UTag.ok => |val| debugPrint("OK with code {d}\n", .{val}),
        UTag.err => unreachable,
    }
    switch (uvar) {
        UTag.ok => |*val| val.* += 100, // if need to update tagged union value
        UTag.err => unreachable,
    }
    debugPrint("OK code plus 100 {d}\n", .{uvar.ok});
}
