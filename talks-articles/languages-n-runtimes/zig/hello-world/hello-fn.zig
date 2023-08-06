const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;
const builtin = @import("builtin");

// pub makes it visible when imported
pub fn sampleFn() !void {
    debugPrint("=================== FUNCTIONS w/ Errors\n", .{});
    //Function
    debugPrint("=> {d}\n", .{mgr_op(sub, 10, 10)});

    // fn reflection
    debugPrint("=> assert: {?}\n", .{@typeInfo(@TypeOf(assert)).Fn.params[0].type.?});
    debugPrint("=> assert: {?}\n", .{@typeInfo(@TypeOf(assert)).Fn.params[0]});
    debugPrint("=> assert: {?}\n", .{@typeInfo(@TypeOf(assert)).Fn.return_type.?});

    //Error
    try errdeferFoo(100);
    errdeferFoo(0) catch |err| {
        debugPrint("error was {?}\n", .{err});
    };
    try sampleMergeError();
}

fn sub(x: u8, y: u8) u8 {
    return x - y;
}

fn rarelyCalled() void {
    @setCold(true);
    debugPrint("Oh, who called\n", .{});
}

// naked calling convention ensures fn to not have any function prologue/epilogue; useful when working with assembly
fn _begin() callconv(.Naked) noreturn {
    rarelyCalled();
}

// inline callconv ensures fn to be inlined at call sites
inline fn oneTwo() u8 {
    return 12;
}

// function pointers are prefixed with *const
const ptr_op = *const fn (a: u8, b: u8) u8;
fn mgr_op(fn_call: ptr_op, o1: u8, o2: u8) u8 {
    return fn_call(add100(o1), o2);
}

fn add100(num: anytype) @TypeOf(num) {
    return num + 100;
}

fn errdeferFoo(num: u8) !void {
    errdefer debugPrint("errdeferFoo got an error.\n", .{});
    if (num == 0) {
        return error.ZeroValue;
    } else {
        debugPrint("errdeferFoo got {d}\n", .{num});
    }
}

fn sampleMergeError() !void {
    const A = error{ AIsBad, UnknownBad };
    const B = error{ BIsBad, UnknownBad };
    const C = A || B; // merging A & B
    debugPrint("{?}\n", .{C.AIsBad});
    debugPrint("{?}\n", .{C.BIsBad});
    debugPrint("{?}\n", .{C.UnknownBad}); // this is from A
}
