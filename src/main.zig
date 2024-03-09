const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    null,
    bool: bool,

    int: i128,
    float: f128,
    // emulating the json
    number_string: []const u8,
    string: []const u8,
    char: u21,
    @"enum": []const u8,

    array: []Value,
    object: std.StringArrayHashMapUnmanaged(Value),
    empty,

    pub fn zonParseFromValue(allocator: Allocator, source: Value, options: ParseOptions) !Value {
        _ = allocator;
        _ = options;
        return source;
    }
};

// shallowly clones array
fn cloned(comptime T: type, alloc: std.mem.Allocator, source: []const T) ![]T {
    const data = try alloc.alloc(T, source.len);
    @memcpy(data, source);
    return data;
}
fn parseToValue(allocator: std.mem.Allocator, source: [:0]const u8, options: ParseOptions) !Value {
    var ast = try std.zig.Ast.parse(allocator, source, .zon);
    defer ast.deinit(allocator);
    const main_index = ast.nodes.get(0).data.lhs;
    return try innerParseToValue(allocator, ast, main_index, options);
}

// reminder for me: reallocate anything that needs allocated as I destroyed it!!!
fn innerParseToValue(allocator: std.mem.Allocator, ast: std.zig.Ast, node_index: std.zig.Ast.Node.Index, options: ParseOptions) !Value {
    const node = ast.nodes.get(node_index);
    switch (node.tag) {
        .array_init_dot_two, .array_init_dot_two_comma, .array_init_dot, .array_init_dot_comma => {
            const arr: []std.zig.Ast.Node.Index = switch (node.tag) {
                .array_init_dot_two, .array_init_dot_two_comma => blk: {
                    var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
                    const res_ast = ast.arrayInitDotTwo(&buf, node_index);
                    const realloced = try allocator.alloc(std.zig.Ast.Node.Index, res_ast.ast.elements.len);
                    @memcpy(realloced, res_ast.ast.elements);
                    break :blk realloced;
                },
                .array_init_dot, .array_init_dot_comma => blk: {
                    const res_ast = ast.arrayInitDot(node_index);
                    const realloced = try allocator.alloc(std.zig.Ast.Node.Index, res_ast.ast.elements.len);
                    @memcpy(realloced, res_ast.ast.elements);
                    break :blk realloced;
                },
                else => unreachable,
            };
            defer allocator.free(arr);
            if (arr.len == 0) return .empty;
            const res_arr = try allocator.alloc(Value, arr.len);
            errdefer allocator.free(res_arr);
            for (0..arr.len) |i| {
                res_arr[i] = try innerParseToValue(allocator, ast, node_index, options);
            }
            return .{ .array = res_arr };
        },
        .struct_init_dot_two, .struct_init_dot_two_comma, .struct_init_dot, .struct_init_dot_comma => {
            const StructField = struct { name: []const u8, expr: std.zig.Ast.Node.Index };
            const stuff: []StructField = switch (node.tag) {
                .struct_init_dot_two, .struct_init_dot_two_comma => blk: {
                    var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
                    const res_ast = ast.structInitDotTwo(&buf, node_index);
                    var fields = try allocator.alloc(StructField, res_ast.ast.fields.len);
                    for (res_ast.ast.fields, 0..) |field, i| {
                        const name_token = ast.firstToken(field) - 2;
                        fields[i] = .{ .name = ast.tokenSlice(name_token), .expr = field };
                    }
                    break :blk fields;
                },
                .struct_init_dot, .struct_init_dot_comma => blk: {
                    const res_ast = ast.structInitDot(node_index);
                    var fields = try allocator.alloc(StructField, res_ast.ast.fields.len);
                    for (res_ast.ast.fields, 0..) |field, i| {
                        const name_token = ast.firstToken(field) - 2;
                        fields[i] = .{ .name = ast.tokenSlice(name_token), .expr = field };
                    }
                    break :blk fields;
                },
                else => unreachable,
            };
            defer allocator.free(stuff);
            var map = std.StringArrayHashMapUnmanaged(Value){};
            errdefer map.deinit(allocator);
            for (stuff) |field| {
                const token = try cloned(u8, allocator, field.name);
                errdefer allocator.free(token);
                const expr = try innerParseToValue(allocator, ast, field.expr, options);
                const res = try map.getOrPutValue(allocator, token, expr);

                if (res.found_existing) {
                    switch (options.duplicate_field_behavior) {
                        .use_first => continue,
                        .@"error" => return error.DuplicateField,
                        .use_last => try map.put(allocator, token, expr),
                    }
                }
            }
            return .{ .object = map };
        },
        .identifier => {
            const token = ast.tokenSlice(node.main_token);
            // debug assert here bc zon syntax SHOULD disallow raw identifiers
            std.debug.assert(std.zig.primitives.isPrimitive(token));
            if (std.mem.eql(u8, token, "true")) {
                return .{ .bool = true };
            } else if (std.mem.eql(u8, token, "false")) {
                return .{ .bool = false };
            } else if (std.mem.eql(u8, token, "null")) {
                return .null;
            } else {
                // no idea what would be valid
                return error.InvalidIdentifier;
            }
        },
        .number_literal => {
            const token = ast.tokenSlice(node.main_token);
            return numberSliceToValue(allocator, token);
        },
        .string_literal => {
            const token = ast.tokenSlice(node.main_token);
            const res = try std.zig.string_literal.parseAlloc(allocator, token);
            return .{ .string = res };
        },
        .char_literal => {
            const token = ast.tokenSlice(node.main_token);
            const res = std.zig.string_literal.parseCharLiteral(token);
            switch (res) {
                .success => |a| return .{ .char = a },
                .failure => return error.InvalidChar,
            }
        },
        .enum_literal => {
            const token = try cloned(u8, allocator, ast.tokenSlice(node.main_token));
            return .{ .@"enum" = token };
        },
        .multiline_string_literal => {
            // TODO: TEST
            var outlit = std.ArrayList(u8).init(allocator);
            defer outlit.deinit();
            for (node.data.lhs..node.data.rhs) |i| {
                const token = ast.tokens.get(i);
                switch (token.tag) {
                    .string_literal => {
                        try outlit.writer().writeAll(ast.tokenSlice(@intCast(i)));
                    },
                    .multiline_string_literal_line => try outlit.writer().writeAll(ast.tokenSlice(@intCast(i))),
                    else => return error.UnexectedToken,
                }
            }
            const data = try std.zig.string_literal.parseAlloc(allocator, outlit.items);
            return .{ .string = data };
        },
        .negation => {
            const expr = ast.nodes.get(node.data.lhs);
            switch (expr.tag) {
                .number_literal => {
                    var data = try numberSliceToValue(allocator, ast.tokenSlice(expr.main_token));
                    switch (data) {
                        .float => |*d| d.* = -d.*,
                        .int => |*i| i.* = -i.*,
                        .number_string => |*s| {
                            defer allocator.free(s.*);
                            const new_data = try allocator.alloc(u8, s.*.len + 1);
                            // TODO: suspicious
                            new_data[0] = '-';
                            @memcpy(new_data[1..], s.*);
                            s.* = new_data;
                        },
                        else => return error.UnexpectedToken,
                    }
                    return data;
                },
                else => return error.UnexpectedToken,
            }
        },
        else => return error.UnexpectedToken,
    }
}

pub fn parseLeaky(comptime T: type, allocator: Allocator, source: anytype, options: ParseOptions) !T {
    var data = std.ArrayList(u8).init(allocator);
    try source.readAllArrayList(&data, std.math.maxInt(usize));
    try data.append(0);
    const res = try data.toOwnedSliceSentinel(0);
    return parseFromSliceLeaky(T, allocator, res, options);
}
pub fn Parsed(comptime T: type) type {
    return struct {
        arena: *std.heap.ArenaAllocator,
        value: T,

        pub fn deinit(self: @This()) void {
            const allocator = self.arena.child_allocator;
            self.arena.deinit();
            allocator.destroy(self.arena);
        }
    };
}
pub fn parse(comptime T: type, allocator: Allocator, source: anytype, options: ParseOptions) !Parsed(T) {
    var parsed = Parsed(T){
        .arena = try allocator.create(std.heap.ArenaAllocator),
        .value = undefined,
    };
    errdefer allocator.destroy(parsed.arena);
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = try parseLeaky(T, parsed.arena.allocator(), source, options);

    return parsed;
}

pub fn parseFromSliceLeaky(comptime T: type, allocator: Allocator, source: [:0]const u8, options: ParseOptions) !T {
    const value = try parseToValue(allocator, source, options);
    return innerParseFromValue(T, allocator, value, options);
}

pub fn parseFromSlice(comptime T: type, allocator: Allocator, source: [:0]const u8, options: ParseOptions) !Parsed(T) {
    var parsed = Parsed(T){ .arena = try allocator.create(std.heap.ArenaAllocator), .value = undefined };
    errdefer allocator.destroy(parsed.arena);
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = try parseFromSliceLeaky(T, parsed.arena.allocator(), source, options);

    return parsed;
}
pub const ParseOptions = struct {
    duplicate_field_behavior: enum {
        use_first,
        @"error",
        use_last,
    } = .@"error",
    ignore_unknown_fields: bool = false,
};
pub fn innerParseFromValue(
    comptime T: type,
    allocator: std.mem.Allocator,
    value: Value,
    options: ParseOptions,
) !T {
    switch (@typeInfo(T)) {
        .Bool => {
            switch (value) {
                .bool => |b| return b,
                else => return error.UnexpectedToken,
            }
        },
        .Float, .ComptimeFloat => {
            return switch (value) {
                .float => |f| @as(T, @floatCast(f)),
                .int => |i| @as(T, @floatFromInt(i)),
                // TODO: number string can be wrong???
                .number_string, .string => |s| std.fmt.parseFloat(T, s),
                else => error.UnexpectedToken,
            };
        },
        .Int, .ComptimeInt => {
            switch (value) {
                .float => |f| {
                    if (@round(f) != f) return error.InvalidNumber;
                    if (f > std.math.maxInt(T)) return error.Overflow;
                    if (f < std.math.minInt(T)) return error.Overflow;
                    return @as(T, @intFromFloat(f));
                },
                .int => |i| {
                    if (i > std.math.maxInt(T)) return error.Overflow;
                    if (i < std.math.minInt(T)) return error.Overflow;
                    return @as(T, @intCast(i));
                },
                .number_string, .string => |s| {
                    return sliceToInt(T, s);
                },
                else => return error.UnexpectedToken,
            }
        },
        .Optional => |optionalInfo| {
            switch (value) {
                .null => return null,
                else => return try innerParseFromValue(optionalInfo.child, allocator, value, options),
            }
        },
        .Enum => {
            if (std.meta.hasFn(T, "zonParseFromValue")) {
                return T.zonParseFromValue(allocator, value, options);
            }

            switch (value) {
                .float => return error.InvalidEnumTag,
                .int => |i| return std.meta.intToEnum(T, i),
                .number_string, .string => |s| return sliceToEnum(T, s),
                .@"enum" => |e| return std.meta.stringToEnum(T, e) orelse error.InvalidEnumTag,
                else => return error.UnexpectedToken,
            }
        },
        .Union => |unionInfo| {
            if (std.meta.hasFn(T, "zonParseFromValue")) {
                return T.zonParseFromValue(allocator, value, options);
            }

            if (unionInfo.tag_type == null) @compileError("Unable to parse into untagged union '" ++ @typeName(T) ++ "'");

            if (value != .object and value != .@"enum") return error.UnexpectedToken;

            const field_name =
                switch (value) {
                .@"enum" => |e| e,
                .object => |o| blk: {
                    var it = o.iterator();
                    const kv = it.next().?;
                    break :blk kv.key_ptr.*;
                },
                else => unreachable,
            };
            inline for (unionInfo.fields) |u_field| {
                if (std.mem.eql(u8, u_field.name, field_name)) {
                    if (u_field.type == void) {
                        // ok ???
                        return @unionInit(T, u_field.name, {});
                    }
                    if (value == .@"enum") return error.UnexpectedToken;
                    return @unionInit(T, u_field.name, try innerParseFromValue(u_field.type, allocator, value, options));
                }
            }

            return error.UnknownField;
        },
        .Struct => |structInfo| {
            if (structInfo.is_tuple) {
                if (value != .array) return error.UnexpectedToken;
                if (value.array.items.len != structInfo.fields.len) return error.UnexpectedToken;

                var r: T = undefined;
                inline for (0..structInfo.fields.len, value.array.items) |i, item| {
                    r[i] = try innerParseFromValue(structInfo.fields[i].type, allocator, item, options);
                }

                return r;
            }

            if (std.meta.hasFn(T, "zonParseFromValue")) {
                return T.zonParseFromValue(allocator, value, options);
            }

            if (value != .object) return error.UnexpectedToken;

            var r: T = undefined;
            var fields_seen = [_]bool{false} ** structInfo.fields.len;

            var it = value.object.iterator();
            while (it.next()) |kv| {
                const field_name = kv.key_ptr.*;

                inline for (structInfo.fields, 0..) |field, i| {
                    if (field.is_comptime) @compileError("comptime fields are not supported: " ++ @typeName(T) ++ "." ++ field.name);
                    if (std.mem.eql(u8, field.name, field_name)) {
                        std.debug.assert(!fields_seen[i]); // TODO: parseToValue check for duplicate
                        @field(r, field.name) = try innerParseFromValue(field.type, allocator, kv.value_ptr.*, options);
                        fields_seen[i] = true;
                        break;
                    }
                } else {
                    if (!options.ignore_unknown_fields) return error.UnknownField;
                }
            }

            try fillDefaultStructValues(T, &r, &fields_seen);
            return r;
        },
        .Array => |arrayInfo| {
            switch (value) {
                .array => |arr| {
                    return innerParseArrayFromArrayValue(T, arrayInfo.child, arrayInfo.len, allocator, arr, options);
                },
                .string => |s| {
                    if (arrayInfo.child != u8) return error.UnexpectedToken;

                    if (s.len != arrayInfo.len) return error.LengthMismatch;

                    var r: T = undefined;
                    @memcpy(r[0..], s);
                    return r;
                },
                else => return error.UnexpectedToken,
            }
        },
        .Vector => |vecInfo| {
            switch (value) {
                .array => |arr| {
                    return innerParseArrayFromArrayValue(T, vecInfo.child, vecInfo.len, allocator, arr, options);
                },
                else => return error.UnexpectedToken,
            }
        },
        .Pointer => |ptrInfo| {
            switch (ptrInfo.size) {
                .One => {
                    const r: *ptrInfo.child = try allocator.create(ptrInfo.child);
                    r.* = try innerParseFromValue(ptrInfo.child, allocator, value, options);
                    return r;
                },
                .Slice => {
                    switch (value) {
                        .array => |arr| {
                            const r = if (ptrInfo.sentinel) |sentinel_ptr|
                                try allocator.allocSentinel(ptrInfo.child, arr.len, @as(*align(1) const ptrInfo.child, @ptrCast(sentinel_ptr)).*)
                            else
                                try allocator.alloc(ptrInfo.child, arr.len);

                            for (arr, r) |item, *dest| {
                                dest.* = try innerParseFromValue(ptrInfo.child, allocator, item, options);
                            }

                            return r;
                        },
                        .string => |s| {
                            if (ptrInfo.child != u8) return error.UnexpectedToken;

                            const r = if (ptrInfo.sentinel) |sentinel_ptr|
                                try allocator.allocSentinel(ptrInfo.child, s.len, @as(*align(1) const ptrInfo.child, @ptrCast(sentinel_ptr)).*)
                            else
                                try allocator.alloc(ptrInfo.child, s.len);
                            @memcpy(r[0..], s);

                            return r;
                        },
                        else => return error.UnexpectedToken,
                    }
                },
                else => @compileError("Unable to parse into type '" ++ @typeName(T) ++ "'"),
            }
        },
        else => @compileError("Unable to parse into type '" ++ @typeName(T) ++ "'"),
    }
}

fn sliceToInt(comptime T: type, slice: []const u8) !T {
    const float = try std.fmt.parseFloat(f128, slice);
    if (@round(float) != float) return error.InvalidNumber;
    if (float > std.math.maxInt(T) or float < std.math.minInt(T)) return error.Overflow;
    return @as(T, @intCast(@as(i128, @intFromFloat(float))));
}

fn sliceToEnum(comptime T: type, slice: []const u8) !T {
    // Check for a named value
    if (std.meta.stringToEnum(T, slice)) |value| return value;

    // why is this public? who knows!
    if (!std.json.isNumberFormattedLikeAnInteger(slice)) return error.InvalidEnumTag;
    const n = std.fmt.parseInt(@typeInfo(T).Enum.tag_type, slice, 10) catch return error.InvalidEnumTag;
    return std.meta.intToEnum(T, n);
}

fn fillDefaultStructValues(comptime T: type, r: *T, fields_seen: *[@typeInfo(T).Struct.fields.len]bool) !void {
    inline for (@typeInfo(T).Struct.fields, 0..) |field, i| {
        if (!fields_seen[i]) {
            if (field.default_value) |default_ptr| {
                const default = @as(*align(1) const field.type, @ptrCast(default_ptr)).*;
                @field(r, field.name) = default;
            } else {
                return error.MissingField;
            }
        }
    }
}

fn numberSliceToValue(allocator: Allocator, slice: []const u8) !Value {
    return .{ .int = std.fmt.parseInt(i128, slice, 0) catch {
        return .{ .float = std.fmt.parseFloat(f128, slice) catch {
            return .{ .number_string = try cloned(u8, allocator, slice) };
        } };
    } };
}

fn innerParseArrayFromArrayValue(
    comptime T: type,
    comptime Child: type,
    comptime len: comptime_int,
    allocator: Allocator,
    array: []Value,
    options: ParseOptions,
) !T {
    if (array.len != len) return error.LengthMismatch;

    var r: T = undefined;
    for (array, 0..) |item, i| {
        r[i] = try innerParseFromValue(Child, allocator, item, options);
    }

    return r;
}

test "simple struct" {
    const TestStruct = struct { a: u8, b: []const u8 };
    const sample_data = ".{ .a = 5, .b = \"hi guys\"}";
    const res = try parseFromSlice(TestStruct, std.testing.allocator, sample_data, .{});
    defer res.deinit();
    try std.testing.expectEqual(5, res.value.a);
    try std.testing.expectEqualStrings("hi guys", res.value.b);
}

test "nested struct" {
    const TestStruct = struct { a: struct { egg: u8, poop: u4 }, b: []const u8 };
    const sample_data = ".{ .a = .{ .egg = 5, .poop = 2 }, .b = \"EGG LOVERS!\"}";
    const res = try parseFromSlice(TestStruct, std.testing.allocator, sample_data, .{});
    defer res.deinit();
    try std.testing.expectEqual(5, res.value.a.egg);
    try std.testing.expectEqual(2, res.value.a.poop);
    try std.testing.expectEqualStrings("EGG LOVERS!", res.value.b);
}

const TestEnum = enum(u8) { one, two, three, four };
test "enum literal" {
    const sample_data = ".two";
    const res = try parseFromSlice(TestEnum, std.testing.allocator, sample_data, .{});
    defer res.deinit();
    try std.testing.expectEqual(TestEnum.two, res.value);
}

test "enum from int" {
    const sample_data = "1";
    const res = try parseFromSlice(TestEnum, std.testing.allocator, sample_data, .{});
    defer res.deinit();
    try std.testing.expectEqual(TestEnum.two, res.value);
}

test "enum from string of name" {
    const sample_data = "\"two\"";
    const res = try parseFromSlice(TestEnum, std.testing.allocator, sample_data, .{});
    defer res.deinit();
    try std.testing.expectEqual(TestEnum.two, res.value);
}

test {
    _ = @import("test.zig");
}
