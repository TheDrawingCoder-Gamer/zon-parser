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

const Aggregates = struct {
    optional: ?i32,
    array: [4]i32,
    vector: @Vector(4, i32),
    pointer: *i32,
    pointer_const: *const i32,
    slice: []i32,
    slice_const: []const i32,
    slice_sentinel: [:0]i32,
    slice_sentinel_const: [:0]const i32,
};

const Enums = enum(u8) {
    zero,
    one,
    two,
    three,
};

const enums_0 = Enums.zero;

const enums_0_doc_0 = ".zero";
const enums_0_doc_1 = "0";
const enums_0_doc_2 = "\"zero\"";

// var so you can take mutable pointers`
var zero: i32 = 0;
const zero_const: i32 = 0;
var array_of_zeros: [4:0]i32 = [_:0]i32{ 0, 0, 0, 0 };
var one: i32 = 1;
const one_const: i32 = 1;
var array_countdown: [4:0]i32 = [_:0]i32{ 4, 3, 2, 1 };

const aggregates_0 = Aggregates{
    .optional = null,
    .array = [4]i32{ 0, 0, 0, 0 },
    .vector = @Vector(4, i32){ 0, 0, 0, 0 },
    .pointer = &zero,
    .pointer_const = &zero_const,
    .slice = array_of_zeros[0..0],
    .slice_const = &[_]i32{},
    .slice_sentinel = array_of_zeros[0..0 :0],
    .slice_sentinel_const = &[_:0]i32{},
};
const aggregates_0_doc =
    \\ .{
    \\ .optional = null,
    \\ .array = .{0,0,0,0},
    \\ .vector = .{0,0,0,0},
    \\ .pointer = 0,
    \\ .pointer_const = 0,
    \\ .slice = .{},
    \\ .slice_const = .{},
    \\ .slice_sentinel = .{},
    \\ .slice_sentinel_const = .{},
    \\ }
;
const aggregates_1 = Aggregates{
    .optional = 1,
    .array = [4]i32{ 1, 2, 3, 4 },
    .vector = @Vector(4, i32){ 1, 2, 3, 4 },
    .pointer = &one,
    .pointer_const = &one_const,
    .slice = array_countdown[0..],
    .slice_const = array_countdown[0..],
    .slice_sentinel = array_countdown[0.. :0],
    .slice_sentinel_const = array_countdown[0.. :0],
};
const aggregates_1_doc =
    \\.{
    \\ .optional = 1,
    \\ .array = .{1, 2, 3, 4},
    \\ .vector = .{1, 2, 3, 4},
    \\ .pointer = 1,
    \\ .pointer_const = 1,
    \\ .slice = .{4, 3, 2, 1},
    \\ .slice_const = .{4, 3, 2, 1},
    \\ .slice_sentinel = .{4, 3, 2, 1},
    \\ .slice_sentinel_const = .{ 4, 3, 2, 1 },
    \\ }
;
pub fn Strings(comptime array_n: usize) type {
    return struct {
        slice_u8: []u8,
        slice_const_u8: []const u8,
        array_u8: [array_n]u8,
        slice_sentinel_u8: [:0]u8,
        slice_const_sentinel_u8: [:0]const u8,
        array_sentinel_u8: [array_n:0]u8,
    };
}
const Strings0 = Strings(4);

var abcd = [4:0]u8{ 'a', 'b', 'c', 'd' };
const strings_0 = Strings0{
    .slice_u8 = abcd[0..],
    .slice_const_u8 = "abcd",
    .array_u8 = [4]u8{ 'a', 'b', 'c', 'd' },
    .slice_sentinel_u8 = abcd[0..],
    .slice_const_sentinel_u8 = "abcd",
    .array_sentinel_u8 = [4:0]u8{ 'a', 'b', 'c', 'd' },
};
const strings_0_doc_0 =
    \\.{
    \\ .slice_u8 = "abcd",
    \\ .slice_const_u8 = "abcd",
    \\ .array_u8 = "abcd",
    \\ .slice_sentinel_u8 = "abcd",
    \\ .slice_const_sentinel_u8 = "abcd",
    \\ .array_sentinel_u8 = "abcd",
    \\ }
;
const strings_0_doc_1 =
    \\.{
    \\ .slice_u8 = .{97, 98, 99, 100},
    \\ .slice_const_u8 = .{97, 98, 99, 100},
    \\ .array_u8 = .{97, 98, 99, 100},
    \\ .slice_sentinel_u8 = .{97, 98, 99, 100},
    \\ .slice_const_sentinel_u8 = .{97, 98, 99, 100},
    \\ .array_sentinel_u8 = .{97, 98, 99, 100},
    \\ }
;
var abcdnewline = [7:0]u8{ 'a', '\n', 'b', '\n', 'c', '\n', 'd' };
const Strings1 = Strings(7);

const strings_1 = Strings1{
    .slice_u8 = abcdnewline[0..],
    .slice_const_u8 =
    \\a
    \\b
    \\c
    \\d
    ,
    .array_u8 = [7]u8{ 'a', '\n', 'b', '\n', 'c', '\n', 'd' },
    .slice_sentinel_u8 = abcdnewline[0..],
    .slice_const_sentinel_u8 =
    \\a
    \\b
    \\c
    \\d
    ,
    .array_sentinel_u8 = [7:0]u8{ 'a', '\n', 'b', '\n', 'c', '\n', 'd' },
};
const strings_1_doc_0 =
    \\.{
    \\  .slice_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\  .slice_const_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\  .array_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\  .slice_sentinel_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\  .slice_const_sentinel_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\  .array_sentinel_u8 =
    \\  \\a
    \\  \\b
    \\  \\c
    \\  \\d
    \\  ,
    \\ }
;
const strings_1_doc_1 =
    \\ .{
    \\ .slice_u8 = "a\nb\nc\nd",
    \\ .slice_const_u8 = "a\nb\nc\nd",
    \\ .array_u8 = "a\nb\nc\nd",
    \\ .slice_sentinel_u8 = "a\nb\nc\nd",
    \\ .slice_const_sentinel_u8 = "a\nb\nc\nd",
    \\ .array_sentinel_u8 = "a\nb\nc\nd",
    \\ }
;

const nums_0: i32 = 65_536;

const nums_0_doc_0 = "65_536";
const nums_0_doc_1 = "0x10000";
const nums_0_doc_2 = "0b10000000000000000";
const nums_0_doc_3 = "0o200000";

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

    try testAllParseFunctions(Aggregates, aggregates_0, aggregates_0_doc);
    try testAllParseFunctions(Aggregates, aggregates_1, aggregates_1_doc);

    try testAllParseFunctions(Strings0, strings_0, strings_0_doc_0);
    try testAllParseFunctions(Strings0, strings_0, strings_0_doc_1);
    try testAllParseFunctions(Strings1, strings_1, strings_1_doc_0);
    try testAllParseFunctions(Strings1, strings_1, strings_1_doc_1);

    try testAllParseFunctions(Enums, enums_0, enums_0_doc_0);
    try testAllParseFunctions(Enums, enums_0, enums_0_doc_1);
    try testAllParseFunctions(Enums, enums_0, enums_0_doc_2);
}

test "number literals" {
    try testAllParseFunctions(i32, nums_0, nums_0_doc_0);
    try testAllParseFunctions(i32, nums_0, nums_0_doc_1);
    try testAllParseFunctions(i32, nums_0, nums_0_doc_2);
    try testAllParseFunctions(i32, nums_0, nums_0_doc_3);
}
