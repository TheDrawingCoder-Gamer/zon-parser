const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
pub usingnamespace @import("stringify.zig");
pub const ast = @import("ast.zig");
pub const ObjectMap = std.StringArrayHashMapUnmanaged(Value);
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
    object: ObjectMap,
    empty,

    pub fn zonParseFromValue(allocator: Allocator, source: Value, options: ParseOptions) !Value {
        _ = allocator;
        _ = options;
        return source;
    }
    pub fn zonParse(allocator: Allocator, tree: Ast, node_index: NodeIndex, options: ParseOptions) !Value {
        return try innerParseToValue(allocator, tree, node_index, options);
    }
    pub fn zonStringify(value: Value, jws: anytype) !void {
        switch (value) {
            .null => try jws.write(null),
            .bool => |inner| try jws.write(inner),
            .int => |inner| try jws.write(inner),
            .float => |inner| try jws.write(inner),
            .number_string => |inner| try jws.print("{s}", .{inner}),
            .string => |inner| try jws.write(inner),
            .array => |inner| try jws.write(inner),
            .object => |inner| {
                try jws.beginObject();
                var it = inner.iterator();
                while (it.next()) |entry| {
                    try jws.objectField(entry.key_ptr.*);
                    try jws.write(entry.value_ptr.*);
                }
                try jws.endObject();
            },
            .empty => {
                try jws.beginArray();
                try jws.endArray();
            },
            // as int (?)
            .char => |inner| try jws.write(inner),
            .@"enum" => |inner| {
                try jws.enumLiteralValue(inner);
            },
        }
    }
};

// shallowly clones array
fn cloned(comptime T: type, alloc: std.mem.Allocator, source: []const T) ![]T {
    const data = try alloc.alloc(T, source.len);
    @memcpy(data, source);
    return data;
}

pub fn innerParse(comptime T: type, allocator: Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index, options: ParseOptions) !T {
    const node = tree.nodes.get(node_index);
    switch (@typeInfo(T)) {
        .Bool => {
            switch (node.tag) {
                .identifier => {
                    const token = tree.tokenSlice(node.main_token);
                    if (std.mem.eql(u8, "true", token)) {
                        return true;
                    } else if (std.mem.eql(u8, "false", token)) {
                        return false;
                    } else {
                        return error.UnexpectedToken;
                    }
                },
                else => return error.UnexpectedToken,
            }
        },
        .Float, .ComptimeFloat, .Int, .ComptimeInt => {
            return numFromAst(T, tree, node_index);
        },
        .Optional => |optionalInfo| {
            switch (node.tag) {
                .identifier => {
                    const token = tree.tokenSlice(node.main_token);
                    if (std.mem.eql(u8, "null", token)) {
                        return null;
                    }
                    // fallout for bools
                },
                else => {},
            }
            return try innerParse(optionalInfo.child, allocator, tree, node_index, options);
        },
        .Enum => {
            if (std.meta.hasFn(T, "zonParse")) {
                return T.zonParse(allocator, tree, node_index, options);
            }

            switch (node.tag) {
                .enum_literal => {
                    const token = tree.tokenSlice(node.main_token);
                    var list = std.ArrayList(u8).init(allocator);
                    defer list.deinit();
                    try std.zig.fmtId(token).format("", .{}, list.writer());
                    return std.meta.stringToEnum(T, list.items) orelse error.InvalidEnumTag;
                },
                else => {},
            }
            return error.UnexpectedToken;
        },
        .Union => |unionInfo| {
            if (std.meta.hasFn(T, "zonParse")) {
                return T.zonParse(allocator, tree, node_index, options);
            }

            if (unionInfo.tag_type == null) @compileError("Unable to parse into untagged union '" ++ @typeName(T) ++ "'");

            const isAnon = isAnonStruct(node.tag);
            const field = field: {
                if (isAnon) {
                    const fields = try structFields(allocator, tree, node_index);
                    defer allocator.free(fields);
                    if (fields) |f| {
                        if (f.len != 1) return error.UnexpectedToken;
                        break :field f[0];
                    } else {
                        return error.UnexpectedToken;
                    }
                } else if (node.tag == .enum_literal) {
                    break :field StructField{ .name = tree.tokenSlice(node_index), .expr = 0 };
                }
                return error.UnexpectedToken;
            };
            inline for (unionInfo.fields) |u_field| {
                if (std.mem.eql(u8, u_field.name, field.name)) {
                    if (u_field.type == void) {
                        if (field.expr != 0) return error.UnexpectedToken; // expected an enum literal, not an object
                        return @unionInit(T, u_field.name, {});
                    }
                    if (field.expr == 0) return error.UnexpectedToken; // expected an object, not an enum literal
                    return @unionInit(T, u_field.name, try innerParse(u_field.type, allocator, tree, field.expr, options));
                }
            }

            return error.UnknownField;
        },
        .Struct => |structInfo| {
            if (structInfo.is_tuple) {
                //
                const nodes = try arrayElemsAlwaysSlice(allocator, tree, node_index);
                defer allocator.free(nodes);
                if (nodes.len != structInfo.fields.len) return error.UnexpectedToken;

                var r: T = undefined;
                // leaky stinky (arena)
                inline for (structInfo.fields, nodes, 0..) |field, n_idx, i| {
                    r[i] = try innerParse(field.type, allocator, tree, n_idx, options);
                }

                return r;
            }

            if (std.meta.hasFn(T, "zonParse")) {
                return T.zonParse(allocator, tree, node_index, options);
            }

            const fields = try structFieldsAlwaysSlice(allocator, tree, node_index);
            defer allocator.free(fields);
            var r: T = undefined;
            var fields_seen = [_]bool{false} ** structInfo.fields.len;

            for (fields) |s_field| {
                inline for (structInfo.fields, 0..) |field, i| {
                    if (field.is_comptime) @compileError("comptime fields are not supported: " ++ @typeName(T) ++ "." ++ field.name);
                    if (std.mem.eql(u8, s_field.name, field.name)) {
                        if (fields_seen[i]) {
                            switch (options.duplicate_field_behavior) {
                                .use_first => break,
                                .@"error" => return error.DuplicateField,
                                .use_last => {},
                            }
                        }
                        @field(r, field.name) = try innerParse(field.type, allocator, tree, s_field.expr, options);
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
            if (arrayInfo.child == u8) {
                return parseStringOrArray(T, arrayInfo.len, allocator, tree, node_index, options);
            } else {
                return internalParseArray(T, arrayInfo.child, arrayInfo.len, allocator, tree, node_index, options);
            }
        },
        .Vector => |vecInfo| {
            return internalParseArray(T, vecInfo.child, vecInfo.len, allocator, tree, node_index, options);
        },
        .Pointer => |ptrInfo| {
            switch (ptrInfo.size) {
                .One => {
                    const r: *ptrInfo.child = try allocator.create(ptrInfo.child);
                    r.* = try innerParse(ptrInfo.child, allocator, tree, node_index, options);
                    return r;
                },
                .Slice => {
                    switch (node.tag) {
                        .string_literal, .multiline_string_literal => {
                            if (ptrInfo.child != u8) return error.UnexpectedToken;

                            if (ptrInfo.sentinel) |sentinel_ptr| {
                                var value_list = std.ArrayList(u8).init(allocator);
                                defer value_list.deinit();
                                try writeString(value_list.writer(), tree, node_index);
                                return try value_list.toOwnedSliceSentinel(@as(*const u8, @ptrCast(sentinel_ptr)).*);
                            }
                            const res = try parseString(allocator, tree, node_index);

                            return res;
                        },
                        else => {
                            const elems = try arrayElemsAlwaysSlice(allocator, tree, node_index);
                            var values =
                                if (ptrInfo.sentinel) |sentinel_ptr|
                                try allocator.allocSentinel(ptrInfo.child, elems.len, @as(*align(1) const ptrInfo.child, @ptrCast(sentinel_ptr)).*)
                            else
                                try allocator.alloc(ptrInfo.child, elems.len);
                            for (elems, 0..) |elem, i| {
                                values[i] = try innerParse(ptrInfo.child, allocator, tree, elem, options);
                            }
                            return values;
                        },
                    }
                },
                else => @compileError("Unable to parse type '" ++ @typeName(T) ++ "'"),
            }
        },

        else => @compileError("Unable to parse type '" ++ @typeName(T) ++ "'"),
    }
}
// reminder for me: reallocate anything that needs allocated as I destroyed it!!!
fn innerParseToValue(allocator: std.mem.Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index, options: ParseOptions) !Value {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .array_init_dot_two, .array_init_dot_two_comma, .array_init_dot, .array_init_dot_comma => {
            const arr: []std.zig.Ast.Node.Index = try arrayElems(allocator, tree, node_index) orelse return .empty;
            defer allocator.free(arr);

            const res_arr = try allocator.alloc(Value, arr.len);
            errdefer allocator.free(res_arr);
            for (0..arr.len) |i| {
                res_arr[i] = try innerParseToValue(allocator, tree, arr[i], options);
            }
            return .{ .array = res_arr };
        },
        .struct_init_dot_two, .struct_init_dot_two_comma, .struct_init_dot, .struct_init_dot_comma => {
            const stuff: []StructField = try structFields(allocator, tree, node_index) orelse return .empty;
            defer allocator.free(stuff);
            var map = std.StringArrayHashMapUnmanaged(Value){};
            errdefer map.deinit(allocator);
            for (stuff) |field| {
                const token = try cloned(u8, allocator, field.name);
                errdefer allocator.free(token);
                const expr = try innerParseToValue(allocator, tree, field.expr, options);
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
            const token = tree.tokenSlice(node.main_token);
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
            const token = tree.tokenSlice(node.main_token);
            return numberSliceToValue(allocator, token);
        },
        .string_literal => {
            const token = tree.tokenSlice(node.main_token);
            const res = try std.zig.string_literal.parseAlloc(allocator, token);
            return .{ .string = res };
        },
        .char_literal => {
            const token = tree.tokenSlice(node.main_token);
            const res = std.zig.string_literal.parseCharLiteral(token);
            switch (res) {
                .success => |a| return .{ .char = a },
                .failure => return error.InvalidChar,
            }
        },
        .enum_literal => {
            const token = try cloned(u8, allocator, tree.tokenSlice(node.main_token));
            return .{ .@"enum" = token };
        },
        .multiline_string_literal => {
            // TODO: TEST
            var outlit = std.ArrayList(u8).init(allocator);
            defer outlit.deinit();
            for (node.data.lhs..node.data.rhs + 1) |i| {
                const token = tree.tokens.get(i);
                switch (token.tag) {
                    .string_literal => {
                        const data = try std.zig.string_literal.parseAlloc(allocator, outlit.items);
                        defer allocator.free(data);
                        try outlit.writer().writeAll(data);
                    },
                    .multiline_string_literal_line => {
                        if (i != node.data.lhs) {
                            try outlit.writer().writeByte('\n');
                        }
                        const slice = tree.tokenSlice(@intCast(i));
                        try outlit.writer().writeAll(slice[2 .. slice.len - 1]);
                    },
                    else => return error.UnexpectedToken,
                }
            }
            return .{ .string = try outlit.toOwnedSlice() };
        },
        .negation => {
            const expr = tree.nodes.get(node.data.lhs);
            switch (expr.tag) {
                .number_literal => {
                    var data = try numberSliceToValue(allocator, tree.tokenSlice(expr.main_token));
                    // LOL
                    errdefer switch (data) {
                        .number_string => |s| allocator.free(s),
                        else => {},
                    };
                    switch (data) {
                        .float => |*d| d.* = -d.*,
                        .int => |*i| i.* = -i.*,
                        .number_string => |*s| {
                            // should this be freed?
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
    // TODO: pull in a custom parsing lib (?)
    var data = std.ArrayList(u8).init(allocator);
    defer data.deinit();
    try source.readAllArrayList(&data, std.math.maxInt(usize));
    const res = try data.toOwnedSliceSentinel(0);
    defer allocator.free(res);
    var tree = try std.zig.Ast.parse(allocator, res, .zon);
    defer tree.deinit(allocator);
    const main_index = tree.nodes.get(0).data.lhs;
    return innerParse(T, allocator, tree, main_index, options);
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
    var tree = try std.zig.Ast.parse(allocator, source, .zon);
    defer tree.deinit(allocator);
    const main_index = tree.nodes.get(0).data.lhs;
    return innerParse(T, allocator, tree, main_index, options);
}

pub fn parseFromSlice(comptime T: type, allocator: Allocator, source: [:0]const u8, options: ParseOptions) !Parsed(T) {
    var parsed = Parsed(T){ .arena = try allocator.create(std.heap.ArenaAllocator), .value = undefined };
    errdefer allocator.destroy(parsed.arena);
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = try parseFromSliceLeaky(T, parsed.arena.allocator(), source, options);

    return parsed;
}

pub fn parseFromValueLeaky(comptime T: type, allocator: Allocator, source: Value, options: ParseOptions) !T {
    return innerParseFromValue(T, allocator, source, options);
}
pub fn parseFromValue(comptime T: type, allocator: Allocator, source: Value, options: ParseOptions) !Parsed(T) {
    var parsed = Parsed(T){
        .arena = try allocator.create(std.heap.ArenaAllocator),
        .value = undefined,
    };
    errdefer allocator.destroy(parsed.arena);
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = try parseFromValueLeaky(T, parsed.arena.allocator(), source, options);

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
                .int, .char => |i| {
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
                // this _can_ leak, but that's why parse and parseFromSlice exist over parseLeaky
                inline for (0..structInfo.fields.len, value.array.items) |i, item| {
                    r[i] = try innerParseFromValue(structInfo.fields[i].type, allocator, item, options);
                }

                return r;
            }

            if (std.meta.hasFn(T, "zonParseFromValue")) {
                return T.zonParseFromValue(allocator, value, options);
            }

            if (value == .empty) {
                var r: T = undefined;
                var fields_seen = [_]bool{false} ** structInfo.fields.len;
                try fillDefaultStructValues(T, &r, &fields_seen);
                return r;
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
                        // can leak but that is why the safe parses exist
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

                    if (s.len != arrayInfo.len) {
                        return error.LengthMismatch;
                    }

                    var r: T = undefined;
                    @memcpy(r[0..], s);
                    return r;
                },
                .empty => {
                    if (arrayInfo.len != 0) return error.UnexpectedToken;
                    return .{};
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
                    errdefer allocator.destroy(r);
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
                            errdefer allocator.free(r);
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
                            // no errors but it never hurts
                            errdefer allocator.free(r);
                            @memcpy(r[0..], s);

                            return r;
                        },
                        .empty => {
                            // Empty slice literals can be cast into mutable slices
                            // unless they're sentinel, as sentinels still need to be alloced even when 0
                            if (ptrInfo.sentinel) |sentinel_ptr| {
                                const r = try allocator.allocSentinel(ptrInfo.child, 0, @as(*align(1) const ptrInfo.child, @ptrCast(sentinel_ptr)).*);
                                return r;
                            } else {
                                return @as(T, &.{});
                            }
                        },
                        else => {
                            return error.UnexpectedToken;
                        },
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

// note: should only ever be used in dynamic (i.e. interacting with Value) code. Otherwise only accept enum literals.
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
const isAnonArray = ast.isAnonArray;
const isAnonStruct = ast.isAnonStruct;
const isEmpty = ast.isEmpty;
const ElemExtractorError = Allocator.Error || error{UnexpectedToken};
const arrayElems = ast.arrayElems;
const arrayElemsAlwaysSlice = ast.arrayElemsAlwaysSlice;
const StructField = struct { name: []const u8, expr: std.zig.Ast.Node.Index };
const structFields = ast.structFields;
const structFieldsAlwaysSlice = ast.structFieldsAlwaysSlice;
pub fn innerParseArrayFromArrayValue(
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
pub fn internalParseArray(
    comptime T: type,
    comptime Child: type,
    comptime len: comptime_int,
    allocator: Allocator,
    tree: std.zig.Ast,
    node_index: std.zig.Ast.Node.Index,
    options: ParseOptions,
) !T {
    const nodes = try arrayElemsAlwaysSlice(allocator, tree, node_index);
    if (nodes.len != len) return error.LengthMismatch;
    var r: T = undefined;
    for (nodes, 0..) |node, i| {
        r[i] = try innerParse(Child, allocator, tree, node, options);
    }

    return r;
}
// assumed u8
pub fn parseStringOrArray(
    comptime T: type,
    comptime len: comptime_int,
    allocator: Allocator,
    tree: std.zig.Ast,
    node_index: std.zig.Ast.Node.Index,
    options: ParseOptions,
) !T {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .string_literal, .multiline_string_literal => {
            const res = try parseString(allocator, tree, node_index);
            defer allocator.free(res);
            if (res.len != len) return error.LengthMismatch;
            var r: T = undefined;
            @memcpy(r[0..], res);
            return r;
        },
        else => return internalParseArray(T, u8, len, allocator, tree, node_index, options),
    }
}
const parseString = ast.parseString;
const writeString = ast.writeString;
const numFromAst = ast.numFromAst;
test {
    _ = @import("test.zig");
}
