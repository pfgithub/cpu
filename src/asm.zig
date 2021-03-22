const std = @import("std");

// ram[0x0] = undefined; // ram[0] is invalid and doesn't exist
// ram[0x1] = instr.li(.r0, 0x79A);
// ram[0x2] = instr.li(.r1, 0x347A);
// ram[0x3] = instr.add(.r0, .r1, .r2);
// ram[0x4] = instr.li(.r3, 1 << 3);
// ram[0x5] = instr.load(.r3, .r0);
// ram[0x6] = instr.li(.r3, 9 << 3); // li .r3 &replace_this_instr
// ram[0x7] = instr.li(.r1, @intCast(u51, instr.li(.r1, 0xC0DE0000)));
// ram[0x8] = instr.store(.r3, .r1);
// ram[0x9] = instr.li(.r1, 0xBAD); // replace_this_instr←
// ram[0xA] = instr.jal(.pc, 2, .r3); // jal pc+2 (&jmp_res)
// ram[0xB] = instr.instruction(0b1111111_0, 0); // (halt)
// ram[0xC] = instr.li(.r2, 0x11C0DE55); // jmp_res←
// ram[0xD] = instr.li(.r0, 0x12);
// ram[0xE] = instr.li(.r1, -0x83);
// ram[0xF] = instr.add(.r0, .r1, .r0);
// ram[0x10] = instr.li(.r5, 0);
// ram[0x11] = instr.add(.r5, .pc, .r3);
// ram[0x12] = instr.instruction(0b1111111_0, 0);

const sample_code = @embedFile("./asm.asm");

const TokenStream = struct {
    index: usize = 0,
    string: []const u8,
    pub fn from(string: []const u8) TokenStream {
        return TokenStream{ .string = string };
    }
    pub fn peek(stream: TokenStream) u8 {
        if (stream.index >= stream.string.len) return 0;
        return stream.string[stream.index];
    }
    pub fn take(stream: *TokenStream) u8 {
        if (stream.index >= stream.string.len) return 0;
        defer stream.index += 1;
        return stream.string[stream.index];
    }
    pub fn startsWithTake(stream: *TokenStream, bit: []const u8) bool {
        if (std.mem.startsWith(u8, stream.string[stream.index..], bit)) {
            stream.index += bit.len;
            return true;
        }
        return false;
    }
};

fn readAny(stream: *TokenStream, comptime fnc: anytype) ?[]const u8 {
    const start = stream.index;
    while (fnc(stream.peek())) {
        _ = stream.take();
    }
    const end = stream.index;
    if (start == end) return null;
    return stream.string[start..end];
}
fn isIdentChar(char: u8) bool {
    return switch (char) {
        'A'...'Z', 'a'...'z', '0'...'9', '-', '_', 128...255 => true,
        else => false,
    };
}
fn isInstrIdentChar(char: u8) bool {
    return switch (char) {
        0...32 => false,
        '[', ']' => false,
        '{', '}' => false,
        '(', ')' => false,
        127 => false,
        else => true, // unicode chars included
    };
}
fn isWhitespace(char: u8) bool {
    return switch (char) {
        ' ', '\n', '\r', '\t' => true,
        else => false,
    };
}
fn readID(stream: *TokenStream) ?[]const u8 {
    // TODO must start with A-Za-z
    return readAny(stream, isIdentChar);
}
fn readInstrID(stream: *TokenStream) ?[]const u8 {
    return readAny(stream, isInstrIdentChar);
}
fn eatWhitespace(stream: *TokenStream) void {
    var li = stream.index;
    while (true) {
        _ = readAny(stream, isWhitespace);
        if (stream.startsWithTake("//")) {
            while (switch (stream.take()) {
                0, '\n' => false,
                else => true,
            }) {}
        }
        if (li == stream.index) break;
        li = stream.index;
    }
}

const AstExpr = struct {
    value: union(enum) {
        instruction: struct {
            name: []const u8,
            args: []AstExpr,
        },
        variable: struct {
            name: []const u8,
        },
        label_ref: struct {
            name: []const u8,
        },
    },
    src: struct { start: usize, end: usize },
    pub fn deinit(expr: AstExpr, alloc: *std.mem.Allocator) void {
        switch (expr.value) {
            .instruction => |instr| {
                for (instr.args) |*arg| arg.deinit(alloc);
                alloc.free(instr.args);
            },
            .variable => {},
            .label_ref => {},
        }
    }
    pub fn parse(alloc: *std.mem.Allocator, data: *Data) error{ ParseError, OutOfMemory }!AstExpr {
        // one of:
        //   (instrname …args)
        //   ±0xHEX
        //   ±0dDEC
        //   varname
        const ts = &data.ts;

        eatWhitespace(ts);
        const start_index = ts.index;
        errdefer ts.index = start_index;

        // check for "±0x"/0d/…

        if (ts.startsWithTake(":")) {
            const id = readID(ts) orelse return parseError(data, start_index, "expected `:label_name`");
            const end_index = ts.index;
            eatWhitespace(ts);

            return AstExpr{
                .src = .{ .start = start_index, .end = end_index },
                .value = .{ .label_ref = .{ .name = id } },
            };
        }

        const id_opt = readID(ts);
        const after_id = ts.index;
        eatWhitespace(ts);

        if (id_opt) |id| {
            return AstExpr{
                .value = .{ .variable = .{ .name = id } },
                .src = .{ .start = start_index, .end = after_id },
            };
        } else switch (ts.peek()) {
            '(' => {
                _ = ts.take();
                eatWhitespace(ts);
                const instr_id = readInstrID(ts) orelse return parseError(data, ts.index, "expected `(instruction_id …args)`");

                var res_exprs = std.ArrayList(AstExpr).init(alloc);
                errdefer {
                    for (res_exprs.items) |*item| item.deinit(alloc);
                    res_exprs.deinit();
                }
                while (true) {
                    eatWhitespace(ts);
                    switch (ts.peek()) {
                        ')' => {
                            _ = ts.take();
                            break;
                        },
                        else => {
                            const expr = try AstExpr.parse(alloc, data);
                            try res_exprs.append(expr);
                        },
                    }
                }
                const end_index = ts.index;
                eatWhitespace(ts);
                return AstExpr{
                    .src = .{ .start = start_index, .end = end_index },
                    .value = .{ .instruction = .{ .name = instr_id, .args = res_exprs.toOwnedSlice() } },
                };
            },
            else => return parseError(data, after_id, "expected ident or `(instruction)`"),
        }
    }
};
const AstDecl = struct {
    value: union(enum) {
        exec: struct {
            out_var: ?[]const u8,
            expr: *AstExpr,
        },
        block: struct {
            decls: []AstDecl,
        },
        label: struct {
            name: []const u8,
        },
    },
    src: struct { start: usize, end: usize },
    pub fn deinit(decl: AstDecl, alloc: *std.mem.Allocator) void {
        switch (decl.value) {
            .exec => |exc| {
                exc.expr.deinit(alloc);
                alloc.destroy(exc.expr);
            },
            .block => |blk| {
                for (blk.decls) |*declv| declv.deinit(alloc);
                alloc.free(blk.decls);
            },
            .label => {},
        }
    }
    pub fn parse(alloc: *std.mem.Allocator, data: *Data) error{ ParseError, OutOfMemory }!AstDecl {
        // one of:
        //   name = expression
        //   label:
        //   { …decl[] }
        const ts = &data.ts;

        eatWhitespace(ts);
        const start_index = ts.index;
        errdefer ts.index = start_index;

        const id_opt = readID(ts);
        eatWhitespace(ts);
        if (id_opt) |id| switch (ts.peek()) {
            '=' => {
                _ = ts.take();
                eatWhitespace(ts);
                // continue
            },
            ':' => {
                _ = ts.take();
                const end_index = ts.index;
                eatWhitespace(ts);
                return AstDecl{
                    .src = .{ .start = start_index, .end = end_index },
                    .value = .{ .label = .{ .name = id } },
                };
            },
            else => return parseError(data, ts.index, "expected `=` or `:`"),
        } else switch (ts.peek()) {
            '{' => {
                // block decl
                _ = ts.take();
                var res_decls = std.ArrayList(AstDecl).init(alloc);
                errdefer {
                    for (res_decls.items) |*item| item.deinit(alloc);
                    res_decls.deinit();
                }
                while (true) {
                    eatWhitespace(ts);
                    switch (ts.peek()) {
                        '}' => {
                            _ = ts.take();
                            break;
                        },
                        else => {
                            const decl = try AstDecl.parse(alloc, data);
                            try res_decls.append(decl);
                        },
                    }
                }
                const end_index = ts.index;
                eatWhitespace(ts);
                return AstDecl{
                    .src = .{ .start = start_index, .end = end_index },
                    .value = .{ .block = .{ .decls = res_decls.toOwnedSlice() } },
                };
            },
            else => {
                // continue
            },
        }

        var slot = try alloc.create(AstExpr);
        errdefer alloc.destroy(slot);

        slot.* = try AstExpr.parse(alloc, data);
        eatWhitespace(ts);

        return AstDecl{
            .src = .{ .start = start_index, .end = slot.src.end },
            .value = .{ .exec = .{ .out_var = id_opt, .expr = slot } },
        };
    }
};
fn parseError(data: *Data, start: usize, msg: []const u8) error{ParseError} {
    if (data.err) |err| err.deinit();
    data.err = Data.Error{
        .start = start,
        .msg = msg,
    };
    return error.ParseError;
}
const Data = struct {
    const Error = struct {
        start: usize,
        msg: []const u8,
        pub fn deinit(me: Error) void {}
    };
    err: ?Error,
    ts: TokenStream,
};

pub fn range(max: usize) []const void {
    return @as([]const void, &[_]void{}).ptr[0..max];
}
pub fn unicodeColumnLen(text: []const u8) usize {
    return @import("lib/wcwidth.zig").wcswidth(text);
}

const Indent = struct {
    count: usize = 0,
    pub fn format(idw: Indent, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        for (range(idw.count)) |_| {
            try out.writeAll("    ");
        }
    }
    pub fn add(a: Indent, b: usize) Indent {
        return Indent{ .count = a.count + b };
    }
};

pub fn printAst(ast: AstDecl, out: anytype, indent: Indent) @TypeOf(out).Error!void {
    switch (ast.value) {
        .block => |blk| {
            try out.writeAll("{\n");
            for (blk.decls) |decl| {
                try out.print("{}", .{indent.add(1)});
                try printAst(decl, out, indent.add(1));
                try out.writeAll("\n");
            }
            try out.print("{}", .{indent});
            try out.writeAll("}");
        },
        .label => |lbl| {
            try out.print("{s}:", .{lbl.name});
        },
        .exec => |exc| {
            if (exc.out_var) |ov| try out.print("{s} = ", .{ov});
            try printAstExpr(exc.expr.*, out, indent);
        },
    }
}

pub fn printAstExpr(ast: AstExpr, out: anytype, indent: Indent) @TypeOf(out).Error!void {
    switch (ast.value) {
        .instruction => |instr| {
            try out.print("({s}", .{instr.name});
            for (instr.args) |arg| {
                try out.writeAll(" ");
                try printAstExpr(arg, out, indent);
            }
            try out.writeAll(")");
        },
        .variable => |varb| {
            try out.print("{s}", .{varb.name});
        },
        .label_ref => |lrf| {
            try out.print(":{s}", .{lrf.name});
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpa.deinit());
    const alloc = &gpa.allocator;

    var data = Data{
        .err = null,
        .ts = TokenStream.from("{" ++ sample_code ++ "}"),
    };
    defer if (data.err) |err| err.deinit();

    const parsed = AstDecl.parse(alloc, &data) catch |e| switch (e) {
        error.ParseError => {
            const err_data = data.err.?;
            const epos = err_data.start;
            const out = std.io.getStdErr().writer();

            var lyn: usize = 0;
            var col: usize = 0;
            var latestLine: usize = 0;
            for (data.ts.string) |char, i| {
                if (epos == i) break;
                col += 1;
                if (char == '\n') {
                    lyn += 1;
                    col = 0;
                    latestLine = i + 1;
                }
            }
            var lineText = std.mem.span(@ptrCast([*:'\n']const u8, &data.ts.string[latestLine]));

            try out.print("./file:{}:{}: {s}\n", .{ lyn + 1, col + 1, err_data.msg }); // todo just save this
            try out.print("{s}\n", .{lineText});
            for (range(unicodeColumnLen(lineText[0..col]))) |_| {
                try out.writeByte(' ');
            }
            try out.writeByte('^');
            try out.writeByte('\n');
            return error.Errored;
        },
        else => return e,
    };
    defer parsed.deinit(alloc);

    if (true) {
        const stdout = std.io.getStdOut().writer();
        try printAst(parsed, stdout, .{});
        try stdout.writeByte('\n');
    }
}
