//! A simple Hello World zig program.
//! to just run: zig run hello-world.zig
//! to build exe: zig build-exe hello-world.zig
//! to create executable at given path: zig build-exe -femit-bin=/tmp/hello hello-world.zig
const std = @import("std");
const debugPrint = std.debug.print;

const hello_values = @import("hello-values.zig");
const hello_strings = @import("hello-strings.zig");
const hello_tests = @import("hello-tests.zig"); // zig test hello-tests.zig
const hello_variables = @import("hello-variables.zig");
const hello_numbers = @import("hello-numbers.zig");
const hello_operators = @import("hello-operators.zig");
const hello_arrays = @import("hello-arrays.zig");
const hello_vectors = @import("hello-vectors.zig");
const hello_pointers = @import("hello-pointers.zig");
const hello_slices = @import("hello-slices.zig");
const hello_structs = @import("hello-structs.zig");
const hello_tuples = @import("hello-tuples.zig");
const hello_enums = @import("hello-enums.zig");
const hello_unions = @import("hello-unions.zig");
const hello_switch = @import("hello-switch.zig");
const hello_while = @import("hello-while.zig");
const hello_for = @import("hello-for.zig");
const hello_if = @import("hello-if.zig");
const hello_defer = @import("hello-defer.zig");
const hello_fn = @import("hello-fn.zig");
const hello_optional = @import("hello-optional.zig");
const hello_casting = @import("hello-casting.zig");
const hello_zerobit = @import("hello-zerobit.zig");
const hello_usingnamespace = @import("hello-usingnamespace.zig");
const hello_comptime = @import("hello-comptime.zig");
const hello_asm = @import("hello-asm.zig");
const hello_mem = @import("hello-mem.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const world = "Zig Land";
    try stdout.print("Hello, {s}!\n", .{world});

    try hello_values.sampleValues();
    try hello_strings.sampleStrings();
    if (hello_tests.fooDetect() == true) {
        std.log.info("check no WARN statements were made", .{});
    }
    try hello_variables.sampleVariables();
    try hello_numbers.sampleNumbers();
    try hello_operators.sampleOp();
    try sampleCompositeType();
    try sampleFlowConstructs();
    try hello_casting.sampleCasting();
    try hello_zerobit.sampleZeroBit();
    try hello_usingnamespace.sampleUsingNamespace();
    try hello_comptime.sampleComptime();
    try hello_asm.sampleAsm();
    try hello_mem.sampleMem();
    debugPrint("done.\n", .{});
}

test "run tests from hello_tests" {
    _ = hello_tests;
}

fn sampleCompositeType() !void {
    try hello_arrays.sampleArrays();
    try hello_vectors.sampleVectors();
    try hello_pointers.samplePointers();
    try hello_slices.sampleSlice();
    try hello_structs.sampleStruct();
    try hello_tuples.sampleTuples();
    try hello_enums.sampleEnums();
    try hello_unions.sampleUnions();
}

fn sampleFlowConstructs() !void {
    try hello_switch.sampleSwitch();
    try hello_while.sampleWhile();
    try hello_for.sampleFor();
    try hello_if.sampleIf();
    try hello_defer.sampleDefer();
    try hello_fn.sampleFn();
    try hello_optional.sampleOptional();
}
