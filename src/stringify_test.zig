const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Value = @import("main.zig").Value;

const ObjectMap = std.StringArrayHashMapUnmanaged(Value);

const StringifyOptions = @import("stringify.zig").StringifyOptions;
const stringify = @import("stringify.zig").stringify;
const stringifyMaxDepth = @import("stringify.zig").stringifyMaxDepth;
const stringifyArbitraryDepth = @import("stringify.zig").stringifyArbitraryDepth;
const stringifyAlloc = @import("stringify.zig").stringifyAlloc;
const writeStream = @import("stringify.zig").writeStream;
const writeStreamMaxDepth = @import("stringify.zig").writeStreamMaxDepth;
const writeStreamArbitraryDepth = @import("stringify.zig").writeStreamArbitraryDepth;

fn getZonObject(allocator: mem.Allocator) !Value {
    var value = Value{ .object = try ObjectMap.init(allocator, &.{}, &.{}) };
    try value.object.put(allocator, "one", Value{ .int = @as(i128, @intCast(1)) });
    try value.object.put(allocator, "two", Value{ .float = 2.0 });
    return value;
}

fn testBasicWriteStream(w: anytype, slice_stream: anytype) !void {
    slice_stream.reset();

    try w.beginObject();

    try w.objectField("object");
    var arena_allocator = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();
    try w.write(try getZonObject(arena_allocator.allocator()));

    try w.objectField("string");
    try w.write(@as([]const u8, "This is a string"));

    try w.objectField("array");
    try w.beginArray();
    try w.write(@as([]const u8, "Another string"));
    try w.write(@as(i32, 1));
    try w.write(@as(f32, 3.5));
    try w.endArray();

    try w.objectField("int");
    try w.write(@as(i32, 10));

    try w.objectField("float");
    try w.write(@as(f32, 3.5));

    try w.objectField("enum");
    try w.write(.egg);

    try w.objectField("invalid_enum");
    try w.write(.@"enum");

    try w.endObject();

    const result = slice_stream.getWritten();
    const expected =
        \\.{
        \\  .object = .{
        \\    .one = 1,
        \\    .two = 2.0e+00,
        \\  },
        \\  .string = "This is a string",
        \\  .array = .{
        \\    "Another string",
        \\    1,
        \\    3.5e+00,
        \\  },
        \\  .int = 10,
        \\  .float = 3.5e+00,
        \\  .@"enum" = .egg,
        \\  .invalid_enum = .@"enum",
        \\}
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "zon write stream" {
    var out_buf: [1024]u8 = undefined;
    var slice_stream = std.io.fixedBufferStream(&out_buf);
    const out = slice_stream.writer();
    {
        var w = writeStream(out, .{ .whitespace = .indent_2 });
        try testBasicWriteStream(&w, &slice_stream);
    }
    {
        var w = writeStreamMaxDepth(out, .{ .whitespace = .indent_2 }, 8);
        try testBasicWriteStream(&w, &slice_stream);
    }
    {
        var w = writeStreamMaxDepth(out, .{ .whitespace = .indent_2 }, null);
        try testBasicWriteStream(&w, &slice_stream);
    }
    {
        var w = writeStreamArbitraryDepth(testing.allocator, out, .{ .whitespace = .indent_2 });
        defer w.deinit();
        try testBasicWriteStream(&w, &slice_stream);
    }
}
