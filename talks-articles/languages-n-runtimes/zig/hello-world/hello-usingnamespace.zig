const S = struct {
    usingnamespace @import("std");
    usingnamespace @import("builtin");
};
const print = S.debug.print;
const os_tag = S.target.os.tag;

pub fn sampleUsingNamespace() !void {
    print("=================== USINGNAMESPACE\n", .{});
    //usingnamespace
    print("tag: {?}\n", .{os_tag});
}
