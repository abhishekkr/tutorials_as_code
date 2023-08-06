const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;
const builtin = @import("builtin");

pub fn sampleSwitch() !void {
    debugPrint("=================== SWITCH\n", .{});
    //Switch
    debugPrint("a_switch | 1 => {}\n", .{a_switch(1)});
    debugPrint("a_switch | 10 => {}\n", .{a_switch(10)});
    debugPrint("a_switch | 11 => {}\n", .{a_switch(11)});
    debugPrint("a_switch | 12 => {}\n", .{a_switch(12)});
    debugPrint("a_switch | 112 => {}\n", .{a_switch(112)});
    debugPrint("a_switch | 152 => {}\n", .{a_switch(152)});
    debugPrint("os_msg => {s}\n", .{os_msg});
    switch_tagged_union();
    try switch_inline();
    assert(switch_inline_for() == ' ');
    switch_inline_else();
}

const os_msg = switch (builtin.target.os.tag) {
    .linux => "LeeNux",
    else => "not Lee",
};

fn a_switch(n: u32) u32 {
    const x: u32 = 12;
    return switch (n) {
        1, 2, 3 => n * 2,
        4...10 => n,
        11 => blk: {
            const n_sq: u32 = n * n;
            break :blk n_sq + n;
        },
        x => x, // allowed as x is known at comptime
        xblk: {
            const y: u32 = 100;
            break :xblk x + y;
        } => x + 10,
        else => 0,
    };
}

fn switch_tagged_union() void {
    const User = struct {
        id: u8,
        name: []const u8,
    };
    const Account = union(enum) {
        guest: bool,
        user: User,
    };

    var jdoe = Account{ .user = User{ .id = 0, .name = "John Doe" } };
    const new_id = 101;
    const jacc = switch (jdoe) {
        Account.guest => &jdoe.user,
        Account.user => |*acc| blk: {
            acc.*.id = new_id;
            break :blk acc;
        },
    };
    debugPrint("jdoe: {d} {s}\n", .{ jacc.id, jacc.name });
}

fn switch_inline() !void {
    const struk_a = struct { a: u8, b: u8, c: ?u8 };
    assert(!try isFieldOptional(struk_a, 0));
    assert(!try isFieldOptional(struk_a, 1));
    assert(try isFieldOptional(struk_a, 2));
    _ = isFieldOptional(struk_a, 3) catch |err| {
        assert(error.IndexOutOfBounds == err);
        return {};
    };
}

fn isFieldOptional(comptime T: type, field_idx: usize) !bool {
    const fields = @typeInfo(T).Struct.fields;
    return switch (field_idx) {
        inline 0...fields.len - 1 => |idx| @typeInfo(fields[idx].type) == .Optional,
        else => return error.IndexOutOfBounds,
    };
}

fn switch_inline_for() u8 {
    const str = "All That Is Said, Is Not Gold.";

    inline for (str) |chr| {
        const isSmallCase = (chr >= 'a' and chr <= 'z');
        const isUpperCase = (chr >= 'A' and chr <= 'Z');
        if (!isSmallCase and !isUpperCase) {
            debugPrint("{c}\n", .{chr});
            return chr;
        }
    }
    unreachable; // as can't be sure that str would have a non-alphabet value
}

fn switch_inline_else() void {
    const Cat = struct {
        const Self = @This();
        pub fn talk(self: Self) void {
            debugPrint("{s} says Meow~\n", .{self.name});
        }

        name: []const u8,
    };
    const Dog = struct {
        const Self = @This();
        pub fn talk(self: Self) void {
            debugPrint("{s} says Bark~\n", .{self.name});
        }

        name: []const u8,
    };

    const Animal = union(enum) {
        const Self = @This();
        pub fn talk(self: Self) void {
            switch (self) {
                inline else => |*case| case.talk(),
            }
        }

        cat: Cat,
        dog: Dog,
    };
    var dartDog: Dog = .{ .name = "Dart" };
    var dart: Animal = .{ .dog = dartDog };
    debugPrint("{any}\n", .{dart});
    dart.talk();
}
