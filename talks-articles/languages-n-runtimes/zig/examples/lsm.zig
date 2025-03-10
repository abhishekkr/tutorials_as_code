const std = @import("std");

const DB_FILEPATH = "data-blah.lsm";
const MAX_MEMORY_SIZE: usize = 512;
const MAX_PREDISK_SIZE: usize = 10240;

var GPA = std.heap.GeneralPurposeAllocator(.{}){};
const ALLOCATOR = GPA.allocator();

const KeyVal = packed struct {
    key: i64,
    val: i64,
};

const MemLayer = struct {
    KV: [MAX_MEMORY_SIZE]KeyVal,
    size: usize,
};

const PreDiskLayer = struct {
    KV: []KeyVal,
    size: usize,

    pub fn persist(self: PreDiskLayer, filepath: []const u8) !void {
        const file = try std.fs.cwd().openFile(filepath, .{ .mode = .write_only });
        defer file.close();
        try file.seekFromEnd(0);

        var buf = std.io.bufferedWriter(file.writer());
        var w = buf.writer();
        for (self.KV) |kv| {
            try w.writeStructEndian(kv, std.builtin.Endian.big);
        }

        try buf.flush();
    }
};

const LSMTree = struct {
    mem: MemLayer,
    predisk: PreDiskLayer,
};

pub fn main() !void {
    const file = try std.fs.cwd().createFile(DB_FILEPATH, .{});
    defer file.close();
    std.debug.print("MAX_MEMORY_SIZE: {d}\n", .{MAX_MEMORY_SIZE});
    var tree = try initLSMTree();

    for (1..999999) |i| {
        const tmp: i64 = @intCast(i);
        try put(&tree, tmp, tmp * 10);
    }

    //printTree(&tree);

    for ([5]i64{ 2, 4, 0, 1998, 1898 }) |k| {
        const value = get(&tree, k) catch |err| {
            std.debug.print("[main] Error: {any}\n", .{err});
            continue;
        };
        std.debug.print("Value: {any}\n", .{value});
    }

    //try printPersisted(DB_FILEPATH);
    _ = printSizes(&tree) catch 0;

    destroyLSMTree(&tree);
    _ = GPA.deinit();
}

fn printSizes(tree: *LSMTree) !void {
    var binf = try std.fs.cwd().openFile(DB_FILEPATH, .{});
    defer binf.close();
    const eof = try binf.getEndPos();
    const persistedSize = @divFloor(eof, @sizeOf(KeyVal));
    std.debug.print("mem.size: {d}\npredisk.size: {d}\ndisk: {d}\n", .{ tree.mem.size, tree.predisk.size, persistedSize });
}

pub fn printPersisted(filepath: []const u8) !void {
    var binf = try std.fs.cwd().openFile(filepath, .{});
    defer binf.close();
    const eof = try binf.getEndPos();
    const binf_reader = binf.reader();
    var currPos = try binf.getPos();
    while (currPos < eof) : (currPos = try binf.getPos()) {
        const kv = try binf_reader.readStructEndian(KeyVal, std.builtin.Endian.big);
        std.debug.print("POS: {any}/{any}\t|\t key: {d}\tval: {d}\n", .{ currPos, eof, kv.key, kv.val });
    }
}

fn initLSMTree() !LSMTree {
    return LSMTree{
        .mem = initMemoryLayer(),
        .predisk = try initPreDiskLayer(),
    };
}

fn initMemoryLayer() MemLayer {
    return MemLayer{
        .KV = undefined,
        .size = 0,
    };
}

fn initPreDiskLayer() !PreDiskLayer {
    var kv: []KeyVal = undefined;
    kv = try ALLOCATOR.alloc(KeyVal, 0);
    errdefer ALLOCATOR.free(kv);
    return PreDiskLayer{
        .KV = kv,
        .size = 0,
    };
}

fn put(tree: *LSMTree, k: i64, v: i64) !void {
    //std.debug.print("tree.mem.size: {d} | {d} => {d}\n", .{ tree.mem.size, k, v });
    if (tree.mem.size >= MAX_MEMORY_SIZE) {
        // Memory layer is full, merge data into predisk layer
        try memToDisk(tree);
    }

    const kv = KeyVal{ .key = k, .val = v };
    tree.mem.KV[tree.mem.size] = kv;
    tree.mem.size += 1;
}

fn memToDisk(tree: *LSMTree) !void {
    //std.debug.print("Moving Mem to PreDisk.\n", .{});
    var oldDiskSize: usize = tree.predisk.size;
    var prediskSize: usize = tree.predisk.size + tree.mem.size;

    //std.debug.print("Increased PreDisk KV from {d} to {d}.\n", .{ tree.predisk.size, prediskSize });
    var prediskKV: []KeyVal = undefined;
    if (prediskSize <= MAX_PREDISK_SIZE) {
        prediskKV = try ALLOCATOR.alloc(KeyVal, prediskSize);
        for (tree.predisk.KV, 0..) |keyVal, idx| {
            prediskKV[idx] = keyVal;
        }
    } else {
        try tree.predisk.persist(DB_FILEPATH);
        //std.debug.print("Current PreDisk Size: {d} NEEDS PERSIST AND TRIM.\n", .{tree.predisk.KV.len});
        oldDiskSize = 0;
        prediskSize = tree.mem.size;
        prediskKV = try ALLOCATOR.alloc(KeyVal, prediskSize);
    }
    ALLOCATOR.free(tree.predisk.KV);
    tree.predisk.KV = prediskKV;

    for (0..tree.mem.size) |idx| {
        tree.predisk.KV[oldDiskSize + idx] = tree.mem.KV[idx];
    }
    tree.predisk.size = tree.predisk.KV.len;

    _ = tree.mem.KV;
    tree.mem = initMemoryLayer();
}

fn get(tree: *LSMTree, k: i64) !i64 {
    for (0..tree.mem.size) |i| {
        if (tree.mem.KV[i].key == k) {
            return tree.mem.KV[i].val;
        }
    }

    for (0..tree.predisk.size) |i| {
        if (tree.predisk.KV[i].key == k) {
            return tree.predisk.KV[i].val;
        }
    }

    const val = getPersisted(DB_FILEPATH, k) catch |err| {
        std.debug.print("Error to find key: {d}\n", .{k});
        std.debug.print("Caught an error: {any}\n", .{err});
        return err;
    };
    return val;

    //return error.Oops; // Value corresponding to the key not found
}

pub fn getPersisted(filepath: []const u8, k: i64) !i64 {
    var binf = try std.fs.cwd().openFile(filepath, .{});
    defer binf.close();
    const eof = try binf.getEndPos();
    const binf_reader = binf.reader();
    var currPos = try binf.getPos();
    while (currPos < eof) : (currPos = try binf.getPos()) {
        const kv = try binf_reader.readStructEndian(KeyVal, std.builtin.Endian.big);
        if (kv.key == k) {
            return kv.val;
        }
    }
    return error.Oops;
}

fn destroyLSMTree(tree: *LSMTree) void {
    _ = tree.mem.KV;
    ALLOCATOR.free(tree.predisk.KV);
}

fn printTree(tree: *LSMTree) void {
    std.debug.print("Memory Layer:\n", .{});
    for (0..tree.mem.size) |i| {
        std.debug.print("Key: {d}, Value: {d}\n", .{ tree.mem.KV[i].key, tree.mem.KV[i].val });
    }

    std.debug.print("PreDisk Layer:\n", .{});
    for (0..tree.predisk.size) |i| {
        std.debug.print("Key: {d}, Value: {d}\n", .{ tree.predisk.KV[i].key, tree.predisk.KV[i].val });
    }
}
