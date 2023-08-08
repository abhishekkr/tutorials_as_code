const std = @import("std");
const debugPrint = std.debug.print;

pub fn sampleAsm() !void {
    debugPrint("=================== ASSEMBLY\n", .{});
    //assmebly
    const msg = "hello zig\n";
    _ = syscall3(SYS_write, STDOUT_FILENO, @intFromPtr(msg), msg.len);
    // _ = syscall1(SYS_exit, 0);
    // unreachable;
}

pub const SYS_write = 1;
pub const SYS_exit = 60;
pub const STDOUT_FILENO = 1;

pub fn syscall1(num: usize, arg1: usize) usize {
    return asm volatile ("syscall"
        : [ret] "={rax}" (-> usize),
        : [num] "{rax}" (num),
          [arg1] "{rdi}" (arg1),
        : "rcx", "r11"
    );
}

pub fn syscall3(num: usize, arg1: usize, arg2: usize, arg3: usize) usize {
    return asm volatile ("syscall"
        : [ret] "={rax}" (-> usize),
        : [num] "{rax}" (num),
          [arg1] "{rdi}" (arg1),
          [arg2] "{rsi}" (arg2),
          [arg3] "{rdx}" (arg3),
        : "rcx", "r11"
    );
}
