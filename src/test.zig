//! tests mostly adapted from std's json tests

const zon_parser = @import("main.zig");
const std = @import("std");
const testing = std.testing;

const Primitives = struct {
    bool: bool,
    f32: f32,
    f64: f64,
    u0: u0,
    i0: i0,
    u1: u1,
    i1: i1,
    u8: u8,
    i8: i8,
    // https://github.com/ziglang/zig/issues/16108
    // i130: i130,
};

const primitives_0 = Primitives{
    .bool = false,
    .f32 = 0,
    .f64 = 0,
    .u0 = 0,
    .i0 = 0,
    .u1 = 0,
    .i1 = 0,
    .u8 = 0,
    .i8 = 0,
    // .i130 = 0,
};

const primitives_0_doc_0 =
    \\.{
    \\ .bool = false,
    \\ .f32 = 0,
    \\ .f64 = 0,
    \\ .u0 = 0,
    \\ .i0 = 0,
    \\ .u1 = 0,
    \\ .i1 = 0,
    \\ .u8 = 0,
    \\ .i8 = 0,
    // \\ .i130 = 0,
    \\ }
;

const primitives_0_doc_1 = // looks like a float
    \\.{
    \\ .bool = false,
    \\ .f32 = 0.0,
    \\ .f64 = 0.0,
    \\ .u0 = 0.0,
    \\ .i0 = 0.0,
    \\ .u1 = 0.0,
    \\ .i1 = 0.0,
    \\ .u8 = 0.0,
    \\ .i8 = 0.0,
    // \\ .i130 = 0.0,
    \\ }
;

const primitives_1 = Primitives{
    .bool = true,
    .f32 = 1073741824,
    .f64 = 1152921504606846976,
    .u0 = 0,
    .i0 = 0,
    .u1 = 1,
    .i1 = -1,
    .u8 = 255,
    .i8 = -128,
    // .i130 = -680564733841876926926749214863536422911,
};

const primitives_1_doc_0 =
    \\.{
    \\ .bool = true,
    \\ .f32 = 1073741824,
    \\ .f64 = 1152921504606846976,
    \\ .u0 = 0,
    \\ .i0 = 0,
    \\ .u1 = 1,
    \\ .i1 = -1,
    \\ .u8 = 255,
    \\ .i8 = -128,
    // \\ .i130 = -680564733841876926926749214863536422911,
    \\ }
;
const primitives_1_doc_1 =
    \\.{
    \\ .bool = true,
    \\ .f32 = 1073741825,
    \\ .f64 = 1152921504606846977,
    \\ .u0 = 0,
    \\ .i0 = 0,
    \\ .u1 = 1,
    \\ .i1 = -1,
    \\ .u8 = 255,
    \\ .i8 = -128,
    // \\ .i130 = -680564733841876926926749214863536422911,
    \\ }
;

fn testAllParseFunctions(comptime T: type, expected: T, doc: [:0]const u8) !void {
    {
        const parsed = try zon_parser.parseFromSlice(T, testing.allocator, doc, .{});
        defer parsed.deinit();
        try testing.expectEqualDeep(expected, parsed.value);
    }
    {
        var stream = std.io.fixedBufferStream(doc);
        const parsed = try zon_parser.parse(T, testing.allocator, stream.reader(), .{});
        defer parsed.deinit();
        try testing.expectEqualDeep(expected, parsed.value);
    }

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    {
        try testing.expectEqualDeep(expected, try zon_parser.parseFromSliceLeaky(T, arena.allocator(), doc, .{}));
    }
    {
        var stream = std.io.fixedBufferStream(doc);
        try testing.expectEqualDeep(expected, try zon_parser.parseLeaky(T, arena.allocator(), stream.reader(), .{}));
    }
}

test "test all types" {
    try testAllParseFunctions(Primitives, primitives_0, primitives_0_doc_0);
    try testAllParseFunctions(Primitives, primitives_0, primitives_0_doc_1);
    try testAllParseFunctions(Primitives, primitives_1, primitives_1_doc_0);
    try testAllParseFunctions(Primitives, primitives_1, primitives_1_doc_1);
}
