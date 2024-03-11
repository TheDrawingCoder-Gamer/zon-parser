//! Convienience functions for working with Zig's AST

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Ast = std.zig.Ast;
pub const NodeIndex = std.zig.Ast.Node.Index;

pub fn parseString(allocator: Allocator, tree: Ast, node_index: NodeIndex) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();
    try writeString(list.writer(), tree, node_index);
    return list.toOwnedSlice();
}

pub fn writeString(writer: anytype, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) !void {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .string_literal => {
            const token = tree.tokenSlice(node.main_token);
            try quickWriteStringLit(writer, token);
        },
        .multiline_string_literal => {
            for (node.data.lhs..node.data.rhs + 1) |i| {
                const token = tree.tokens.get(i);
                switch (token.tag) {
                    .string_literal => {
                        const data = tree.tokenSlice(@intCast(i));
                        try quickWriteStringLit(writer, data);
                    },
                    .multiline_string_literal_line => {
                        if (i != node.data.lhs) {
                            try writer.writeByte('\n');
                        }
                        const slice = tree.tokenSlice(@intCast(i));
                        try writer.writeAll(slice[2 .. slice.len - 1]);
                    },
                    else => return error.UnexpectedToken,
                }
            }
        },
        else => return error.UnexpectedToken,
    }
}

fn quickWriteStringLit(writer: anytype, bytes: []const u8) !void {
    switch (try std.zig.string_literal.parseWrite(writer, bytes)) {
        .success => {},
        .failure => return error.InvalidLiteral,
    }
}

fn genericParseNum(comptime T: type, bytes: []const u8) !T {
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {
            return std.fmt.parseInt(T, bytes, 0);
        },
        .Float, .ComptimeFloat => {
            return std.fmt.parseFloat(T, bytes);
        },
        else => @compileError("T must be an Int or a Float, got '" ++ @typeName(T) ++ "'"),
    }
}

pub fn numFromAst(comptime T: type, tree: Ast, node_index: NodeIndex) !T {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .negation => {
            // fuck it we ball !!
            // brainrot core
            switch (@typeInfo(T)) {
                .Int => |intInfo| {
                    switch (intInfo.signedness) {
                        // not compile error - unsigned are accepted without negation
                        .unsigned => return error.Overflow,
                        .signed => {
                            const SlightlyBiggerT = @Type(.{ .Int = .{ .signedness = .signed, .bits = intInfo.bits + 1 } });

                            const child_node = tree.nodes.get(node.data.lhs);
                            // If we were to directly call into numFromAst, the compiler would likely have to generate all the functions
                            // likely causing a segfault
                            if (child_node.tag != .number_literal) return error.UnexpectedToken;
                            const res = -try std.fmt.parseInt(SlightlyBiggerT, tree.tokenSlice(child_node.main_token), 0);
                            if (res < std.math.minInt(T)) return error.Overflow;
                            if (res > std.math.maxInt(T)) return error.Overflow;
                            return @as(T, @intCast(res));
                        },
                    }
                },
                else => {
                    return -try numFromAst(T, tree, node.data.lhs);
                },
            }
        },
        .number_literal => {
            const token = tree.tokenSlice(node.main_token);
            return genericParseNum(T, token);
        },
        // be quiet
        .char_literal => {
            switch (@typeInfo(T)) {
                .Int, .ComptimeInt => {
                    const token = tree.tokenSlice(node.main_token);
                    switch (std.zig.string_literal.parseCharLiteral(token)) {
                        .success => |c| {
                            if (c < std.math.minInt(T)) return error.Overflow;
                            if (c > std.math.maxInt(T)) return error.Overflow;
                            return @as(T, @intCast(c));
                        },
                        .failure => return error.InvalidLiteral,
                    }
                },
                else => return error.UnexpectedToken,
            }
        },
        else => return error.UnexpectedToken,
    }
    unreachable;
}

pub fn isAnonArray(tree: std.zig.Ast, idx: std.zig.Ast.Node.Index) bool {
    return switch (tree.nodes.get(idx).tag) {
        .array_init_dot, .array_init_dot_comma, .array_init_dot_two, .array_init_dot_two_comma => true,
        else => isEmpty(tree, idx),
    };
}
pub fn isAnonStruct(tree: std.zig.Ast, idx: std.zig.Ast.Node.Index) bool {
    return switch (tree.nodes.get(idx).tag) {
        .struct_init_dot, .struct_init_dot_comma, .struct_init_dot_two, .struct_init_dot_two_comma => true,
        else => isEmpty(tree, idx),
    };
}
pub fn isEmpty(tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) bool {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .array_init_dot, .array_init_dot_comma => {
            const res_ast = tree.arrayInitDot(node_index);
            if (res_ast.ast.elements.len == 0) return true;
        },
        .array_init_dot_two, .array_init_dot_two_comma => {
            var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
            const res_ast = tree.arrayInitDotTwo(&buf, node_index);
            if (res_ast.ast.elements.len == 0) return true;
        },
        .struct_init_dot, .struct_init_dot_comma => {
            const res_ast = tree.structInitDot(node_index);
            if (res_ast.ast.fields.len == 0) return true;
        },
        .struct_init_dot_two, .struct_init_dot_two_comma => {
            var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
            const res_ast = tree.structInitDotTwo(&buf, node_index);
            if (res_ast.ast.fields.len == 0) return true;
        },
        else => {},
    }
    return false;
}
pub const ElemExtractorError = Allocator.Error || error{UnexpectedToken};

pub fn arrayElems(allocator: Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) ElemExtractorError!?[]std.zig.Ast.Node.Index {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .array_init_dot_comma, .array_init_dot => {
            const res_ast = tree.arrayInitDot(node_index);
            if (res_ast.ast.elements.len == 0) return null;
            const realloced = try allocator.alloc(std.zig.Ast.Node.Index, res_ast.ast.elements.len);
            @memcpy(realloced, res_ast.ast.elements);
            return realloced;
        },
        .array_init_dot_two, .array_init_dot_two_comma => {
            var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
            const res_ast = tree.arrayInitDotTwo(&buf, node_index);
            if (res_ast.ast.elements.len == 0) return null;
            const realloced = try allocator.alloc(std.zig.Ast.Node.Index, res_ast.ast.elements.len);
            @memcpy(realloced, res_ast.ast.elements);
            return realloced;
        },
        else => {
            if (isEmpty(tree, node_index)) {
                return null;
            }
        },
    }
    return error.UnexpectedToken;
}
pub fn arrayElemsAlwaysSlice(allocator: Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) ElemExtractorError![]std.zig.Ast.Node.Index {
    if (try arrayElems(allocator, tree, node_index)) |elems| {
        return elems;
    }
    return &.{};
}
pub const StructField = struct { name: []const u8, expr: std.zig.Ast.Node.Index };
pub fn structFields(allocator: Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) ElemExtractorError!?[]StructField {
    const node = tree.nodes.get(node_index);
    switch (node.tag) {
        .struct_init_dot, .struct_init_dot_comma => {
            const res_ast = tree.structInitDot(node_index);
            if (res_ast.ast.fields.len == 0) return null;
            var fields = try allocator.alloc(StructField, res_ast.ast.fields.len);
            for (res_ast.ast.fields, 0..) |field, i| {
                const name_token = tree.firstToken(field) - 2;
                fields[i] = .{ .name = tree.tokenSlice(name_token), .expr = field };
            }
            return fields;
        },
        .struct_init_dot_two, .struct_init_dot_two_comma => {
            var buf = [_]std.zig.Ast.Node.Index{0} ** 2;
            const res_ast = tree.structInitDotTwo(&buf, node_index);
            if (res_ast.ast.fields.len == 0) return null;
            var fields = try allocator.alloc(StructField, res_ast.ast.fields.len);
            for (res_ast.ast.fields, 0..) |field, i| {
                const name_token = tree.firstToken(field) - 2;
                fields[i] = .{ .name = tree.tokenSlice(name_token), .expr = field };
            }
            return fields;
        },
        else => {
            if (isEmpty(tree, node_index)) {
                return null;
            }
        },
    }
    return error.UnexpectedToken;
}
pub fn structFieldsAlwaysSlice(allocator: Allocator, tree: std.zig.Ast, node_index: std.zig.Ast.Node.Index) ElemExtractorError![]StructField {
    if (try structFields(allocator, tree, node_index)) |fields| {
        return fields;
    }
    return &.{};
}
