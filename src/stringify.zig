const std = @import("std");
const Allocator = std.mem.Allocator;
const BitStack = std.BitStack;
const assert = std.debug.assert;
pub const StringifyOptions = struct {
    whitespace: enum {
        minified,
        indent_1,
        indent_2,
        indent_3,
        indent_4,
        indent_8,
        indent_tab,
    } = .minified,
    /// Should optional fields with a null value be written?
    emit_null_optional_fields: bool = true,
    /// Arrays/slices of u8 are typically encoded as strings.
    /// This encodes them as arrays of numbers instead.
    emit_strings_as_arrays: bool = false,
    /// Should unicode characters be escaped in strings?
    escape_unicode: bool = false,
};

pub fn stringify(
    value: anytype,
    options: StringifyOptions,
    out_stream: anytype,
) @TypeOf(out_stream).Error!void {
    var jw = writeStream(out_stream, options);
    defer jw.deinit();
    try jw.write(value);
}

/// pass null to disable safety checks
pub fn stringifyMaxDepth(
    value: anytype,
    options: StringifyOptions,
    out_stream: anytype,
    comptime max_depth: ?usize,
) @TypeOf(out_stream).Error!void {
    var jw = writeStreamMaxDepth(out_stream, options, max_depth);
    try jw.write(value);
}
pub fn stringifyArbitraryDepth(
    allocator: Allocator,
    value: anytype,
    options: StringifyOptions,
    out_stream: anytype,
) !void {
    var jw = writeStreamArbitraryDepth(allocator, out_stream, options);
    defer jw.deinit();
    try jw.write(value);
}

pub fn stringifyAlloc(
    allocator: Allocator,
    value: anytype,
    options: StringifyOptions,
) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();
    try stringifyArbitraryDepth(allocator, value, options, list.writer());
    return list.toOwnedSlice();
}
pub fn writeStream(
    out_stream: anytype,
    options: StringifyOptions,
) WriteStream(@TypeOf(out_stream), .{ .checked_to_fixed_depth = 256 }) {
    return writeStreamMaxDepth(out_stream, options, 256);
}
pub fn writeStreamMaxDepth(
    out_stream: anytype,
    options: StringifyOptions,
    comptime max_depth: ?usize,
) WriteStream(
    @TypeOf(out_stream),
    if (max_depth) |d| .{ .checked_to_fixed_depth = d } else .assumed_correct,
) {
    return WriteStream(
        @TypeOf(out_stream),
        if (max_depth) |d| .{ .checked_to_fixed_depth = d } else .assumed_correct,
    ).init(undefined, out_stream, options);
}
pub fn writeStreamArbitraryDepth(
    allocator: Allocator,
    out_stream: anytype,
    options: StringifyOptions,
) WriteStream(@TypeOf(out_stream), .checked_to_arbitrary_depth) {
    return WriteStream(@TypeOf(out_stream), .checked_to_arbitrary_depth).init(allocator, out_stream, options);
}
const OBJECT_MODE = 0;
const ARRAY_MODE = 1;
// copied mostly from std.json but outputing zon instead of json
pub fn WriteStream(
    comptime OutStream: type,
    comptime safety_checks_hint: union(enum) {
        checked_to_arbitrary_depth,
        checked_to_fixed_depth: usize,
        assumed_correct,
    },
) type {
    return struct {
        const Self = @This();
        const safety_checks: @TypeOf(safety_checks_hint) = switch (@import("builtin").mode) {
            .Debug, .ReleaseSafe => safety_checks_hint,
            .ReleaseFast, .ReleaseSmall => .assumed_correct,
        };

        pub const Stream = OutStream;
        pub const Error = switch (safety_checks) {
            .checked_to_arbitrary_depth => Stream.Error || error{OutOfMemory},
            .checked_to_fixed_depth, .assumed_correct => Stream.Error,
        };

        options: StringifyOptions,

        stream: OutStream,
        indent_level: usize = 0,
        next_punctuation: enum {
            the_beginning,
            none,
            comma,
            equals,
        } = .the_beginning,

        nesting_stack: switch (safety_checks) {
            .checked_to_arbitrary_depth => BitStack,
            .checked_to_fixed_depth => |fixed_buffer_size| [(fixed_buffer_size + 7) >> 3]u8,
            .assumed_correct => void,
        },

        pub fn init(safety_allocator: Allocator, stream: OutStream, options: StringifyOptions) Self {
            return .{
                .options = options,
                .stream = stream,
                .nesting_stack = switch (safety_checks) {
                    .checked_to_arbitrary_depth => BitStack.init(safety_allocator),
                    .checked_to_fixed_depth => |fixed_buffer_size| [_]u8{0} ** ((fixed_buffer_size + 7) >> 3),
                    .assumed_correct => {},
                },
            };
        }

        pub fn deinit(self: *Self) void {
            switch (safety_checks) {
                .checked_to_arbitrary_depth => self.nesting_stack.deinit(),
                .checked_to_fixed_depth, .assumed_correct => {},
            }
            // ???
            self.* = undefined;
        }
        pub fn print(self: *Self, comptime fmt: []const u8, args: anytype) Error!void {
            try self.valueStart();
            try self.stream.print(fmt, args);
            self.valueDone();
        }
        fn pushIdentation(self: *Self, mode: u1) !void {
            switch (safety_checks) {
                .checked_to_arbitrary_depth => {
                    try self.nesting_stack.push(mode);
                    self.indent_level += 1;
                },
                .checked_to_fixed_depth => {
                    BitStack.pushWithStateAssumeCapacity(&self.nesting_stack, &self.indent_level, mode);
                },
                .assumed_correct => {
                    self.indent_level += 1;
                },
            }
        }
        fn popIdentation(self: *Self, should_be: u1) void {
            switch (safety_checks) {
                .checked_to_arbitrary_depth => {
                    assert(self.nesting_stack.pop() == should_be);
                    self.indent_level -= 1;
                },
                .checked_to_fixed_depth => {
                    assert(BitStack.popWithState(&self.nesting_stack, &self.indent_level) == should_be);
                },
                .assumed_correct => {
                    self.indent_level -= 1;
                },
            }
        }
        fn valueStart(self: *Self) !void {
            if (self.isObjectKeyExpected()) |is_it| assert(!is_it); // call objectField(), not write, for object keys
            return self.valueStartAssumeTypeOk();
        }
        fn objectFieldStart(self: *Self) !void {
            if (self.isObjectKeyExpected()) |is_it| assert(is_it); // call write() not objectField()
            return self.valueStartAssumeTypeOk();
        }
        fn indent(self: *Self) !void {
            var char: u8 = ' ';
            const n_chars = switch (self.options.whitespace) {
                .minified => return,
                .indent_1 => 1 * self.indent_level,
                .indent_2 => 2 * self.indent_level,
                .indent_3 => 3 * self.indent_level,
                .indent_4 => 4 * self.indent_level,
                .indent_8 => 8 * self.indent_level,
                .indent_tab => blk: {
                    char = '\t';
                    break :blk self.indent_level;
                },
            };
            try self.stream.writeByte('\n');
            try self.stream.writeByteNTimes(char, n_chars);
        }
        fn valueStartAssumeTypeOk(self: *Self) !void {
            assert(!self.isComplete());
            switch (self.next_punctuation) {
                .the_beginning => {},
                .none => {
                    try self.indent();
                },
                .comma => {
                    try self.stream.writeByte(',');
                    try self.indent();
                },
                .equals => {
                    if (self.options.whitespace != .minified) {
                        try self.stream.writeByte(' ');
                    }
                    try self.stream.writeByte('=');
                    if (self.options.whitespace != .minified) {
                        try self.stream.writeByte(' ');
                    }
                },
            }
        }
        fn valueDone(self: *Self) void {
            self.next_punctuation = .comma;
        }
        fn isComplete(self: *const Self) bool {
            return self.indent_level == 0 and self.next_punctuation == .comma;
        }
        fn isObjectKeyExpected(self: *const Self) ?bool {
            switch (safety_checks) {
                .checked_to_arbitrary_depth => return self.indent_level > 0 and
                    self.nesting_stack.peek() == OBJECT_MODE and
                    self.next_punctuation != .equals,
                .checked_to_fixed_depth => return self.indent_level > 0 and
                    BitStack.peekWithState(&self.nesting_stack, self.indent_level) == OBJECT_MODE and
                    self.next_punctuation != .equals,
                .assumed_correct => return null,
            }
        }

        pub fn beginArray(self: *Self) Error!void {
            try self.valueStart();
            try self.stream.writeAll(".{");
            try self.pushIdentation(ARRAY_MODE);
            self.next_punctuation = .none;
        }
        pub fn beginObject(self: *Self) Error!void {
            try self.valueStart();
            try self.stream.writeAll(".{");
            try self.pushIdentation(OBJECT_MODE);
            self.next_punctuation = .none;
        }
        pub fn endArray(self: *Self) Error!void {
            self.popIdentation(ARRAY_MODE);
            switch (self.next_punctuation) {
                .none => {},
                .comma => {
                    // add trailing comma for zig fmt
                    if (self.options.whitespace != .minified) {
                        try self.stream.writeByte(',');
                    }
                    try self.indent();
                },
                .the_beginning, .equals => unreachable,
            }
            try self.stream.writeByte('}');
            self.valueDone();
        }
        pub fn endObject(self: *Self) Error!void {
            self.popIdentation(OBJECT_MODE);
            switch (self.next_punctuation) {
                .none => {},
                .comma => {
                    // add trailing comma for zig fmt
                    if (self.options.whitespace != .minified) {
                        try self.stream.writeByte(',');
                    }
                    try self.indent();
                },
                .the_beginning, .equals => unreachable,
            }
            try self.stream.writeByte('}');
            self.valueDone();
        }
        pub fn objectField(self: *Self, key: []const u8) Error!void {
            try self.objectFieldStart();
            try self.stream.writeByte('.');
            try std.zig.fmtId(key).format("{}", .{}, self.stream);
            self.next_punctuation = .equals;
        }
        pub fn enumLiteralValue(self: *Self, s: []const u8) !void {
            try self.valueStart();
            try self.stream.writeByte('.');
            try std.zig.fmtId(s).format("{}", .{}, self.stream);
            self.valueDone();
        }
        fn stringValue(self: *Self, s: []const u8) !void {
            try self.valueStart();
            try self.stream.writeByte('"');
            try std.zig.fmtEscapes(s).format("", .{}, self.stream);
            try self.stream.writeByte('"');
            self.valueDone();
        }
        pub fn write(self: *Self, value: anytype) Error!void {
            const T = @TypeOf(value);
            switch (@typeInfo(T)) {
                .Int => {
                    try self.valueStart();
                    try self.stream.print("{}", .{value});
                    self.valueDone();
                    return;
                },
                .ComptimeInt => {
                    return self.write(@as(std.math.IntFittingRange(value, value), value));
                },
                .Float, .ComptimeFloat => {
                    if (@as(f128, @floatCast(value)) == value) {
                        try self.valueStart();
                        try self.stream.print("{}", .{@as(f128, @floatCast(value))});
                        self.valueDone();
                        return;
                    }

                    try self.valueStart();
                    try self.stream.print("\"{}\"", .{value});
                    self.valueDone();
                    return;
                },
                .Bool => {
                    try self.valueStart();
                    try self.stream.writeAll(if (value) "true" else "false");
                    self.valueDone();
                    return;
                },
                .Null => {
                    try self.valueStart();
                    try self.stream.writeAll("null");
                    self.valueDone();
                    return;
                },
                .Optional => {
                    if (value) |payload| {
                        return try self.write(payload);
                    } else {
                        return try self.write(null);
                    }
                },
                .Enum, .EnumLiteral => {
                    if (std.meta.hasFn(T, "zonStringify")) {
                        return value.zonStringify(self);
                    }
                    return self.enumLiteralValue(@tagName(value));
                },
                .Union => {
                    if (std.meta.hasFn(T, "zonStringify")) {
                        return value.zonStringify(self);
                    }
                    const info = @typeInfo(T).Union;
                    if (info.tag_type) |UnionTagType| {
                        inline for (info.fields) |u_field| {
                            if (value == @field(UnionTagType, u_field.name)) {
                                if (u_field.type == void) {
                                    try self.valueStart();
                                    try self.stream.writeByte('.');
                                    try std.zig.fmtId(u_field.name).format("{}", .{}, self.stream);
                                    self.valueDone();
                                } else {
                                    try self.beginObject();
                                    try self.objectField(u_field.name);
                                    try self.write(@field(value, u_field.name));
                                    try self.endObject();
                                }
                                break;
                            }
                        } else {
                            unreachable; // no active tag?
                        }
                        return;
                    } else {
                        @compileError("Unable to stringify untagged union '" ++ @typeName(T) ++ "'");
                    }
                },
                .Struct => |S| {
                    if (std.meta.hasFn(T, "zonStringify")) {
                        return value.zonStringify(self);
                    }

                    if (S.is_tuple) {
                        try self.beginArray();
                    } else {
                        try self.beginObject();
                    }
                    inline for (S.fields) |Field| {
                        if (Field.type == void) continue;

                        var emit_field = true;

                        if (@typeInfo(Field.type) == .Optional) {
                            if (self.options.emit_null_optional_fields == false) {
                                if (@field(value, Field.name) == null) {
                                    emit_field = false;
                                }
                            }
                        }

                        if (emit_field) {
                            if (!S.is_tuple) {
                                try self.objectField(Field.name);
                            }
                            try self.write(@field(value, Field.name));
                        }
                    }
                    if (S.is_tuple) {
                        try self.endArray();
                    } else {
                        try self.endObject();
                    }
                    return;
                },
                .ErrorSet => @compileError("Unable to stringify error set '" ++ @typeName(T) ++ "'"),
                .Pointer => |ptr_info| switch (ptr_info.size) {
                    .One => switch (@typeInfo(ptr_info.child)) {
                        .Array => {
                            const Slice = []const std.meta.Elem(ptr_info.child);
                            return self.write(@as(Slice, value));
                        },
                        else => {
                            return self.write(value.*);
                        },
                    },
                    .Many, .Slice => {
                        if (ptr_info.size == .Many and ptr_info.sentinel == null)
                            @compileError("unable to stringify type '" ++ @typeName(T) ++ "' without sentinel");
                        const slice = if (ptr_info.size == .Many) std.mem.span(value) else value;

                        if (ptr_info.child == u8) {
                            if (!self.options.emit_strings_as_arrays and std.unicode.utf8ValidateSlice(slice)) {
                                return self.stringValue(slice);
                            }
                        }

                        try self.beginArray();
                        for (slice) |x| {
                            try self.write(x);
                        }
                        try self.endArray();
                        return;
                    },
                    else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'"),
                },
                .Array => {
                    // coerce [N]T to *const [N]T (which gets converted to a slice)
                    return self.write(&value);
                },
                .Vector => |info| {
                    const array: [info.len]info.child = value;
                    return self.write(&array);
                },
                else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'"),
            }
            unreachable;
        }
    };
}

test {
    _ = @import("stringify_test.zig");
}
