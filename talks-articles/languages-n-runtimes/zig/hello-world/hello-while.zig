const std = @import("std");
const debugPrint = std.debug.print;
const assert = std.debug.assert;

pub fn sampleWhile() !void {
    debugPrint("=================== WHILE\n", .{});
    //While
    var idx: usize = 0;
    while (idx < 100) {
        debugPrint("{d} =>", .{idx});
        idx += 1;
        if (idx > 50)
            break;
        if (idx % 2 == 0)
            continue;
        idx += 8;
    }
    debugPrint("{d}\n", .{idx});
    assert(idx == 51);

    sampleWhileContinue();
    const retVal1 = sampleWhileReturn(3);
    const retVal2 = sampleWhileReturn(11);
    debugPrint("retVal for 3: {d} | retVal for 11: {d}\n", .{ retVal1, retVal2 });
    sampleWhileLabel();

    sampleWhileOptionals();
    sampleWhileError();
    sampleWhileInline();
}

fn sampleWhileContinue() void {
    var idx: usize = 0;
    while (idx < 10) : (idx += 1) {} // loop continue expression
    assert(idx == 10);

    var idy: usize = 0;
    while (idx < 100 and idy < 10) : ({
        idx += 1;
        idy += 1;
    }) {
        debugPrint("{d}: {d} =>", .{ idx, idy });
    }
    debugPrint("{d}: {d}\n", .{ idx, idy });
}

fn sampleWhileReturn(num: u8) u8 {
    var idx: u8 = 0;
    var retVal: u8 = 0;
    return while (idx < 10) : (idx += 1) {
        retVal += idx;
        if (num == idx) {
            break retVal;
        }
    } else num;
}

fn sampleWhileLabel() void {
    var i: usize = 0;
    outer: while (i < 10) : (i += 1) {
        while (true) {
            continue :outer; // would allow to not be infinite
        }
    }
    assert(i == 10);
}

fn sampleWhileOptionals() void {
    var counter: u8 = 3;
    while (countdown(counter)) |v| {
        debugPrint("optional {?}\n", .{v});
        counter = v;
    } else {
        debugPrint("optional break at {?}\n", .{counter});
    }
}

fn countdown(num: u8) ?u8 {
    return if (num == 0) null else num - 1;
}

fn sampleWhileError() void {
    var counter: u8 = 3;
    while (countdownToError(counter)) |v| {
        debugPrint("err-union {?}\n", .{v});
        counter = v;
    } else |err| {
        debugPrint("error {?} at {?}\n", .{ err, counter });
    }
}

fn countdownToError(num: u8) anyerror!u8 {
    return if (num == 0) error.ReachedZero else num - 1;
}

fn sampleWhileInline() void {
    comptime var idx = 0;
    var sum: usize = 0;
    inline while (idx < 5) : (idx += 1) {
        const T: type = typeNames(idx);
        const typeLen = @typeName(T).len;
        debugPrint("len {?}: {d}\n", .{ T, typeLen });
        sum += typeLen;
    }
    debugPrint("type len sum: {d}\n", .{sum});
}

fn typeNames(comptime idx: u8) type {
    return switch (idx) {
        0 => f32,
        1 => i8,
        3 => u8,
        2 => bool,
        4 => union { a: bool, b: u32 },
        else => unreachable,
    };
}
