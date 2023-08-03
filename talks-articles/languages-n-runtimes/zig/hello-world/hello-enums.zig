const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

const Status = enum {
    ok,
    warning,
    critical,
};

const init_status = Status.warning;

const StatusWithIdx = enum(u2) {
    ok,
    not_ok,
};

const StatusWithOverrides = enum(u8) {
    ok = 0,
    not_ok = 2,
    undef,

    pub fn isUndef(self: StatusWithOverrides) bool {
        return self == StatusWithOverrides.undef;
    }
};

pub fn sampleEnums() !void {
    debugPrint("============= ENUMS", .{});
    // Enum
    debugPrint("{?}\n", .{init_status});
    debugPrint("StatusWithIdx.ok: {?}\n", .{@intFromEnum(StatusWithIdx.ok)});
    debugPrint("StatusWithIdx.not_ok: {?}\n", .{@intFromEnum(StatusWithIdx.not_ok)});
    debugPrint("StatusWithOverrides.not_ok: {?}\n", .{@intFromEnum(StatusWithOverrides.not_ok)});
    debugPrint("StatusWithOverrides.undef: {?}\n", .{@intFromEnum(StatusWithOverrides.undef)});

    var blah = StatusWithOverrides.undef;
    debugPrint("is blah undef: {}\n", .{blah.isUndef()});

    // switch it
    const undef_msg = switch (blah) {
        StatusWithOverrides.ok => "it is ok",
        .not_ok => "it is not ok", // can be just used without type's name
        StatusWithOverrides.undef => "it is not known",
    };
    debugPrint("blah means {s}\n", .{undef_msg});

    debugPrint("type info of Status: {?}\n", .{@typeInfo(Status).Enum.tag_type});
    debugPrint("fields count of Status: {?}\n", .{@typeInfo(Status).Enum.fields.len});
    debugPrint("fields entity [1] of Status: {s}\n", .{@typeInfo(Status).Enum.fields[1].name});

    // using without type's name
    const status_a: Status = .ok;
    assert(status_a == Status.ok);

    // non-exhaustive enums
    const Count = enum(u8) {
        one,
        two,
        three,
        _,
    };
    const num = Count.one;
    debugPrint("{?}\n", .{num});
    const word = switch (num) {
        .one => "One",
        .two => "Two",
        .three => "Three",
        _ => "Greater than Three",
    };
    debugPrint("{?} is {s}\n", .{ num, word });

    const is_two = switch (num) {
        .two => true,
        else => false,
    };
    debugPrint("{?} == Two is {?}\n", .{ num, is_two });
}
