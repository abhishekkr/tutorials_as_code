const std = @import("std");
const debugPrint = std.debug.print;

const User = struct {
    firstname: []const u8,
    lastname: []const u8,

    pub fn char_count(self: User) u64 {
        return self.firstname.len + self.lastname.len;
    }
};

const Roles = struct {
    pub const admin = "Admin";
    pub const guest = "Guest";
};

const AnB = struct {
    a: u32,
    b: u32,
    pub fn init(a: u32, b: u32) AnB {
        return AnB{
            .a = a,
            .b = b,
        };
    }
};

fn setThisUsingThat(a: *u32, b: u32) void {
    const anb = @fieldParentPtr(AnB, "a", a);
    anb.b = b;
}

pub fn sampleStruct() !void {
    debugPrint("=================== STRUCTS\n", .{});
    const johndoe = User{
        .firstname = "John",
        .lastname = undefined,
    };
    debugPrint("{s} {s}\n", .{ johndoe.firstname, johndoe.lastname });
    debugPrint("{d}\n", .{User.char_count(johndoe)});

    debugPrint("{s} | size: {d} | empty struct: {any}\n", .{ Roles.admin, @sizeOf(Roles), Roles{} });

    var ab = AnB.init(100, 200);
    setThisUsingThat(&ab.a, 101);
    debugPrint("{d}\n", .{ab.b});

    try sampleLinkedList();
    try samplePackedStruct();
}

fn LinkedList(comptime T: type) type { // way to do Generics in Zig
    return struct {
        const Self = @This();
        pub const Node = struct {
            prev: ?*Node = null,
            next: ?*Node = null,
            data: T,
        };
        pub fn append(list: *Self, new_node: *Node) void {
            if (list.len > 0) {
                list.last.?.next = new_node;
            } else {
                list.first = new_node;
            }
            new_node.prev = list.last;
            list.last = new_node;
            list.len += 1;
        }

        first: ?*Node = null,
        last: ?*Node = null,
        len: usize = 0,
    };
}

fn sampleLinkedList() !void {
    // a comptime func calls are memoized
    try std.testing.expect(LinkedList(i32) == LinkedList(i32));

    const i32_lnklist = LinkedList(i32);
    var i32_list = i32_lnklist{};
    try std.testing.expect(i32_list.len == 0);
    debugPrint("first: {?}, last: {?}, len: {?}\n", .{ i32_list.first, i32_list.last, i32_list.len });

    var node1 = i32_lnklist.Node{ .data = 100 };
    i32_list.append(&node1);
    debugPrint("first: {d}, last: {d}\n", .{ i32_list.first.?.data, i32_list.last.?.data });

    var node2 = i32_lnklist.Node{ .data = 101 };
    i32_list.append(&node2);
    debugPrint("first: {d}, last: {d}\n", .{ i32_list.first.?.data, i32_list.last.?.data });
}

pub fn samplePackedStruct() !void {
    const ABC = packed struct {
        a: u1,
        b: u2,
        c: u3,
    };
    var abc = ABC{
        .a = 1,
        .b = 2,
        .c = 5,
    };
    std.debug.assert(@intFromPtr(&abc.a) == @intFromPtr(&abc.b));
    std.debug.assert(@intFromPtr(&abc.a) == @intFromPtr(&abc.c));
    debugPrint("abc struct type is {s}\n", .{@typeName(ABC)});
    debugPrint("anon struct{{}} type is {s}\n", .{@typeName(struct {})});
    debugPrint("LinkedList(i32) type is {s}\n", .{@typeName(LinkedList(i32))});

    var anb: AnB = .{
        .a = 0,
        .b = 1,
    };
    debugPrint("AnB struct is {?}\n", .{@TypeOf(anb)});
    // anonymous struct literal
    var anon = .{
        .a = 0,
        .b = 1,
    };
    debugPrint("anon AnB style struct is {?}\n", .{@TypeOf(anon)});
}
