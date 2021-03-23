const std = @import("std");

// "oh I'll just make a simple assembly language with register allocation"
// "it won't be that complicated"
// "what do you mean you've done this once before and it was that complicated?"

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
fn isIdentStartChar(char: u8) bool {
    return switch (char) {
        'A'...'Z', 'a'...'z' => true,
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
    const start = stream.index;
    if (isIdentStartChar(stream.peek())) {
        _ = stream.take();
    } else return null;
    while (isIdentChar(stream.peek())) {
        _ = stream.take();
    }
    const end = stream.index;
    return stream.string[start..end];
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

pub fn readNumber(data: *Data) !?i64 {
    const ts = &data.ts;
    const start_index = ts.index;
    errdefer ts.index = start_index;

    // alternatively:
    // if ts.peek() is '+', '-', '0'...'9' => parseInt(i64, ident, 0);
    // switch (ts.peek) {
    //     '+', '-', '0'...'9' => {},
    //     else => return null,
    // }

    const signed = ts.startsWithTake("-");

    if (ts.startsWithTake("0")) {
        const radix: u8 = switch (ts.take()) {
            'x' => 16,
            'd' => 10,
            'b' => 2,
            'o' => 8,
            else => return parseError(data, start_index, "Expected number eg 0x5 or 0d23"),
        };
        const number = readInstrID(ts) orelse return parseError(data, start_index, "Expected number eg 0x5 or 0d23");
        const res = std.fmt.parseInt(i63, number, radix) catch |e| switch (e) {
            error.Overflow => return parseError(data, start_index, "Number is too big; does not fit in an i63"),
            error.InvalidCharacter => return parseError(data, start_index, "Number is invalid; Expected eg 0x5 or 0d23"),
        };
        return if (signed) -res else res;
    } else {
        if (signed) return parseError(data, start_index, "Expected number eg -0x5 or -0d23");
        return null;
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
        reg: struct {
            name: []const u8,
        },
        number: i64,
    },
    src: Src,
    pub fn deinit(expr: AstExpr, alloc: *std.mem.Allocator) void {
        switch (expr.value) {
            .instruction => |instr| {
                for (instr.args) |*arg| arg.deinit(alloc);
                alloc.free(instr.args);
            },
            .variable => {},
            .label_ref => {},
            .reg => {},
            .number => {},
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
        if (ts.startsWithTake("#")) {
            const id = readID(ts) orelse return parseError(data, start_index, "expected `#reg_name`");
            const end_index = ts.index;
            eatWhitespace(ts);

            return AstExpr{
                .src = .{ .start = start_index, .end = end_index },
                .value = .{ .reg = .{ .name = id } },
            };
        }
        if (try readNumber(data)) |nv| {
            const end_index = ts.index;
            eatWhitespace(ts);
            return AstExpr{
                .src = .{ .start = start_index, .end = end_index },
                .value = .{ .number = nv },
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
            else => return parseError(data, after_id, "expected variable, `0xNUMBER`, or `(instruction)`"),
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
    src: Src,
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
        .reg => |lrf| {
            try out.print("#{s}", .{lrf.name});
        },
        .number => |num| {
            try out.print("{d}", .{num});
        },
    }
}

// const IR = struct {};
// IR needs to be able to handle jumps properly

// 0b0000001_0

const RegisterSpace = enum { normal };

const ImmediateValue = struct {
    width: std.math.Log2Int(u64),
    value: union(enum) {
        constant: u64,
        label: LabelDefinition, // resolves to the offset between pc and the label
    },
};
const Arg = union(enum) {
    none: void,
    register: VariableDefinition,
    immediate: ImmediateValue,
    out_reg: VariableDefinition,
};

const IR_Block = struct {
    instructions: []IR_Instruction,
};

const Src = struct { start: usize, end: usize };

const SystemRegister = enum(u4) {
    pc = 0b1111,
};
const VariableDefinition = struct {
    value: union(enum) {
        allocated: SystemRegister,
        unallocated: struct {
            definition_src: Src, // where the variable was defined
            id: usize, // a unique id that represents this variable
        },
    },
    space: RegisterSpace, // if this register is an int register or a float register eg
};
const LabelDefinition = struct {
    definition_src: Src,
    id: usize,
};

const Scope = struct {
    alloc: *std.mem.Allocator,
    parent_scope: ?*Scope,

    variables: std.StringHashMap(VariableDefinition),
    labels: std.StringHashMap(LabelDefinition),

    pub fn new(parent_scope: *Scope) !*Scope {
        var res = try newBase(parent_scope.alloc);
        errdefer res.deinit();

        res.parent_scope = parent_scope;

        return res;
    }
    pub fn newBase(alloc: *std.mem.Allocator) !*Scope {
        var scope = try alloc.create(Scope);
        errdefer alloc.destroy(scope);

        scope.* = .{
            .alloc = alloc,
            .parent_scope = null,

            .variables = std.StringHashMap(VariableDefinition).init(alloc),
            .labels = std.StringHashMap(LabelDefinition).init(alloc),
        };

        return scope;
    }
    pub fn destroy(scope: *Scope) void {
        scope.variables.deinit();
        scope.labels.deinit();
        const alloc = scope.alloc;
        alloc.destroy(scope);
    }
    pub fn getVariable(scope: *Scope, varname: []const u8) ?VariableDefinition {
        if (scope.variables.get(varname)) |res| return res //
        else if (scope.parent_scope) |parent| return parent.getVariable(varname) //
        else return null;
    }
    pub fn defVariable(scope: *Scope, name: []const u8, vardef: VariableDefinition) !void {
        try scope.variables.putNoClobber(name, vardef);
    }
    pub fn getLabel(scope: *Scope, labelname: []const u8) ?LabelDefinition {
        if (scope.labels.get(labelname)) |res| return res //
        else if (scope.parent_scope) |parent| return parent.getLabel(labelname) //
        else return null;
    }
    pub fn defLabel(scope: *Scope, name: []const u8, lbldef: LabelDefinition) !void {
        try scope.labels.putNoClobber(name, lbldef);
    }
};

var global_id_value: usize = 0;
fn nextID() usize {
    defer global_id_value += 1;
    return global_id_value;
}

const IrgenData = struct {
    const Error = struct {
        src: Src,
        msg: []const u8,
        pub fn deinit(me: Error) void {}
    };
    err: ?Error, // this can just be : Error = undefined // nevermind it can't
};

const IrgenError = error{ IrgenError, OutOfMemory };
fn irgenError(data: *IrgenData, src: Src, msg: []const u8) IrgenError {
    if (data.err) |prev_err| prev_err.deinit();
    data.err = .{
        .src = src,
        .msg = msg,
    };
    return IrgenError.IrgenError;
}

pub fn irgen(data: *IrgenData, parent_scope: *Scope, outer_block: []AstDecl, out_block: *std.ArrayList(IR_Instruction)) IrgenError!void {
    const scope = try Scope.new(parent_scope);
    defer scope.destroy();

    // 1: find and predefine labels

    for (outer_block) |decl| switch (decl.value) {
        .exec => |exec| {},
        .block => {},
        .label => |label| {
            if (scope.getLabel(label.name)) |prev_label| {
                const res = irgenError(data, decl.src, "redefinition of label. TODO note previous definition.");
                return res;
            }
            try scope.defLabel(label.name, LabelDefinition{
                .definition_src = decl.src,
                .id = nextID(),
            });
        },
    };

    // 2: irgen and define variables
    // oops these are like python variables. TODO fix, python variables are bad.
    // cheat way to fix: `:=` vs `=` - there that's what I'll do probably because there are no keywords in this language

    for (outer_block) |decl| switch (decl.value) {
        .exec => |exec| {
            const out_var: ?VariableDefinition = blk: { // this control flow is a bit messy and confusing
                const ovname = exec.out_var orelse break :blk null;
                break :blk scope.getVariable(ovname) orelse {
                    const vardef = VariableDefinition{
                        .value = .{
                            .unallocated = .{
                                .definition_src = decl.src, // TODO exec.out_var_src?
                                .id = nextID(),
                            },
                        },
                        .space = .normal,
                    };
                    try scope.defVariable(ovname, vardef);
                    break :blk vardef;
                };
            };

            try irgenReg(data, scope, out_var, exec.expr.*, out_block);
        },
        .block => |block| {
            try irgen(data, scope, block.decls, out_block);
        },
        .label => |lbl| {
            const label: LabelDefinition = scope.getLabel(lbl.name) orelse unreachable;
            try out_block.append(IR_Instruction{
                .src = decl.src,
                .value = .{
                    .jump_label = label,
                },
            });
        },
    };
}

const IR_Instruction = struct {
    value: union(enum) {
        // normal instruction sets have a few standard instruction types
        // this doesn't really so all instructions must be able to fit in here
        standard_instr: struct {
            instr_id: InstructionID,
            args: [InstructionMaxArgsCount]Arg,
        },
        jump_label: LabelDefinition,
        // jmp_instr
    },
    src: Src,
    pub fn deinit(instr: IR_Instruction) void {}
};

const InstructionID = enum(u8) {
    noop = 0b0000000_0,
    li = 0b0000001_0,
    add = 0b0000010_0,
    load = 0b0000011_0,
    store = 0b0000100_0,
    jal = 0b0000101_0,
    halt = 0b1111111_0,
};
const InstructionMaxArgsCount = 4;

const InstrInfo = struct {
    const ArgSpec = union(enum) {
        constant: struct { width: std.math.Log2Int(u64), value: u64 },
        reg: struct { name: []const u8, space: RegisterSpace },
        out: struct { space: RegisterSpace },
        immediate: struct { name: []const u8, width: std.math.Log2Int(u64), signed: bool },
    };
    const InstructionSpec = struct {
        instr_id: InstructionID,
        args: [InstructionMaxArgsCount]ArgSpec,
    };
    fn constant(comptime Width: type, value: Width) ArgSpec {
        const ti: std.builtin.TypeInfo.Int = @typeInfo(Width).Int;
        const bit_count = ti.bits + switch (ti.signedness) {
            .unsigned => 0,
            .signed => 1,
        };
        return ArgSpec{
            .constant = .{
                .width = bit_count,
                .value = @bitCast(std.meta.Int(.unsigned, bit_count), value),
            },
        };
    }
    fn out(space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .out = .{ .space = space },
        };
    }
    fn immediate(name: []const u8, comptime Width: type) ArgSpec {
        const ti: std.builtin.TypeInfo.Int = @typeInfo(Width).Int;
        return ArgSpec{
            .immediate = .{ .name = name, .width = std.meta.bitCount(Width), .signed = ti.signedness == .signed },
        };
    }
    fn reg(name: []const u8, space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .reg = .{ .name = name, .space = space },
        };
    }
    fn instr(comptime id: InstructionID, comptime args: []const ArgSpec) InstructionSpec {
        comptime {
            var res_spec: []const ArgSpec = args;
            if (res_spec.len > InstructionMaxArgsCount) @compileError("Too many spec items. Max is InstructionMaxArgsCount");

            var bit_count: comptime_int = 8;
            for (res_spec) |itm| {
                bit_count += @as(comptime_int, switch (itm) {
                    .constant => |cns| cns.width,
                    .reg => 4,
                    .out => 4,
                    .immediate => |imm| imm.width,
                });
            }
            if (bit_count != 64) @compileError("Does not add up to 64 bits.");

            while (res_spec.len < InstructionMaxArgsCount) {
                res_spec = res_spec ++ &[_]ArgSpec{constant(u0, 0)};
            }

            return InstructionSpec{
                .instr_id = id,
                .args = res_spec[0..InstructionMaxArgsCount].*,
            };
        }
    }

    pub const instructions = std.ComptimeStringMap(InstructionSpec, .{
        .{ "noop", instr(.noop, &[_]ArgSpec{constant(u56, 0)}) },
        .{ "li", instr(.li, &[_]ArgSpec{ out(.normal), immediate("value", i52) }) },
        .{ "add", instr(.add, &[_]ArgSpec{ reg("lhs", .normal), reg("rhs", .normal), out(.normal), constant(u44, 0) }) },
        .{ "load", instr(.load, &[_]ArgSpec{ reg("addr", .normal), out(.normal), constant(u48, 0) }) },
        .{ "store", instr(.store, &[_]ArgSpec{ reg("addr", .normal), reg("value", .normal), constant(u48, 0) }) },
        .{ "halt", instr(.halt, &[_]ArgSpec{constant(u56, 0)}) },
    });
};

pub fn irgenIntermediate(data: *IrgenData, scope: *Scope, space: RegisterSpace, expr: AstExpr, out_block: *std.ArrayList(IR_Instruction)) IrgenError!VariableDefinition {
    switch (expr.value) {
        .variable => |vbl| {
            return scope.getVariable(vbl.name) orelse {
                return irgenError(data, expr.src, "Variable not found");
            };
        },
        .reg => {
            return VariableDefinition{
                .value = .{
                    .allocated = .pc,
                },
                .space = .normal,
            };
        },
        else => {
            var slot: VariableDefinition = .{
                .value = .{
                    .unallocated = .{
                        .definition_src = expr.src, // TODO exec.out_var_src?
                        .id = nextID(),
                    },
                },
                .space = space,
            };
            try irgenReg(data, scope, slot, expr, out_block);
            return slot;
        },
    }
}

fn SliceIterator(comptime SliceType: type) type {
    return struct {
        slice: SliceType,
        index: usize = 0,
        const This = @This();
        pub fn next(iter: *This) ?std.meta.Child(SliceType) {
            if (iter.index >= iter.slice.len) return null;
            defer iter.index += 1;
            return iter.slice[iter.index];
        }
    };
}

fn sliceIterator(slice: anytype) SliceIterator(@TypeOf(slice)) {
    const ResType = SliceIterator(@TypeOf(slice));
    return ResType{ .slice = slice };
}

pub fn irgenReg(data: *IrgenData, scope: *Scope, out_reg: ?VariableDefinition, expr: AstExpr, out_block: *std.ArrayList(IR_Instruction)) IrgenError!void {
    // instruction: struct {
    //     name: []const u8,
    //     args: []AstExpr,
    // },
    // variable: struct {
    //     name: []const u8,
    // },
    // label_ref: struct {
    //     name: []const u8,
    // },
    switch (expr.value) {
        .instruction => |instr| {
            const spec: InstrInfo.InstructionSpec = InstrInfo.instructions.get(instr.name) orelse {
                return irgenError(data, expr.src, "No instruction exists with this name"); // TODO instr.name_src?
            };
            // const Arg = union(enum) {
            //     none: void,
            //     register: VariableDefinition,
            //     immediate: struct {
            //         width: std.math.Log2Int(u64),
            //         value: u64,
            //     },
            //     out_reg: VariableDefinition,
            // };

            // const IR_Instruction = struct {
            //     value: union(enum) {
            //         // normal instruction sets have a few standard instruction types
            //         // this doesn't really so all instructions must be able to fit in here
            //         standard_instr: struct {
            //             instr_id: InstructionID,
            //             args: [InstructionMaxArgsCount]Arg,
            //         },
            //         jump_label: LabelDefinition,
            //         // jmp_instr
            //     },
            //     src: Src,
            // };

            // create an iterator over instr.args
            var args = sliceIterator(instr.args);

            var res_args: [InstructionMaxArgsCount]Arg = [_]Arg{undefined} ** InstructionMaxArgsCount;
            if (spec.args.len != InstructionMaxArgsCount) unreachable;
            for (spec.args) |spec_arg, i| {
                res_args[i] = switch (spec_arg) {
                    .constant => |cns| Arg{
                        .immediate = .{
                            .width = cns.width,
                            .value = .{
                                .constant = cns.value,
                            },
                        },
                    },
                    .reg => |reg| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .register = try irgenIntermediate(data, scope, reg.space, arg, out_block),
                        };
                    },
                    .out => |out| blk: {
                        break :blk Arg{
                            .out_reg = out_reg orelse {
                                return irgenError(data, expr.src, "Return value is ignored");
                            },
                        };
                    },
                    .immediate => |imm| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .immediate = try irgenImmediate(data, scope, imm.width, imm.signed, arg, out_block),
                        };
                    },
                };
            }
            if (args.next()) |nxt_arg| return irgenError(data, nxt_arg.src, "Extra argument");

            try out_block.append(IR_Instruction{
                .src = expr.src,
                .value = .{
                    .standard_instr = .{
                        .instr_id = spec.instr_id,
                        .args = res_args,
                    },
                },
            });
        },
        .variable => |varbl| {
            return irgenError(data, expr.src, "A register doesn't fit here.");
        },
        .label_ref => {
            return irgenError(data, expr.src, "An immediate value doesn't fit here.");
        },
        .reg => {
            return irgenError(data, expr.src, "A register doesn't fit here.");
        },
        .number => {
            return irgenError(data, expr.src, "An immediate value doesn't fit here.");
        },
    }
}

fn runtimeBitcast(num: i64, width: std.math.Log2Int(u64), signed: bool) ?u64 {
    const mask = @as(u64, std.math.maxInt(u64)) << width;
    const num_bcd = @bitCast(u64, num);

    if (signed) {
        const sbit = @as(u64, std.math.maxInt(u64)) << (width - 1);
        if (num_bcd & mask == 0 and num_bcd & sbit == 0) {
            return num_bcd;
        } else if (num_bcd & mask == mask and num_bcd & sbit == sbit) {
            return num_bcd & ~mask;
        } else {
            return null;
        }
    } else {
        if (mask & num_bcd == 0) return num_bcd;
        return null;
    }
}

test "" {
    std.testing.expectEqual(runtimeBitcast(25, 8, false), 25);
    std.testing.expectEqual(runtimeBitcast(-25, 8, false), null);
    std.testing.expectEqual(runtimeBitcast(255, 8, false), 255);
    std.testing.expectEqual(runtimeBitcast(255, 8, true), null);
    std.testing.expectEqual(runtimeBitcast(127, 8, true), 127);
    std.testing.expectEqual(runtimeBitcast(128, 8, true), null);
    std.testing.expectEqual(runtimeBitcast(-128, 8, true), @bitCast(u8, @as(i8, -128)));
    std.testing.expectEqual(runtimeBitcast(-129, 8, false), null);
    std.testing.expectEqual(runtimeBitcast(-129, 9, true), @bitCast(u9, @as(i9, -129)));
}

// OutWidth: std.meta.Int(…, …)
pub fn irgenImmediate(data: *IrgenData, scope: *Scope, width: std.math.Log2Int(u64), signed: bool, expr: AstExpr, out_block: *std.ArrayList(IR_Instruction)) IrgenError!ImmediateValue {
    switch (expr.value) {
        .instruction => |instr| {
            return irgenError(data, expr.src, "An instruction doesn't fit here. This slot is for an immediate value.");
        },
        .variable => |instr| {
            return irgenError(data, expr.src, "An instruction doesn't fit here. This slot is for an immediate value.");
        },
        .label_ref => |lbl_ref| {
            return ImmediateValue{
                .width = width,
                .value = .{
                    .label = scope.getLabel(lbl_ref.name) orelse {
                        return irgenError(data, expr.src, "Label not found.");
                    },
                },
            };
        },
        .reg => |lbl_ref| {
            return irgenError(data, expr.src, "A register doesn't fit here. This slot is for an immediate value.");
        },
        .number => |num| {
            // uuh uuh
            // runtime bitcast num to `{signed ? "i" : "u"}{width}`
            return ImmediateValue{
                .width = width,
                .value = .{
                    .constant = runtimeBitcast(num, width, signed) orelse {
                        return irgenError(data, expr.src, "This value doesn't fit in this slot");
                    },
                },
            };
        },
    }
}

pub fn printReportedError(start: usize, msg: []const u8, code: []const u8) !void {
    const epos = start;
    const out = std.io.getStdErr().writer();

    var lyn: usize = 0;
    var col: usize = 0;
    var latestLine: usize = 0;
    for (code) |char, i| {
        if (epos == i) break;
        col += 1;
        if (char == '\n') {
            lyn += 1;
            col = 0;
            latestLine = i + 1;
        }
    }
    var lineText = std.mem.span(@ptrCast([*:'\n']const u8, &code[latestLine]));

    try out.print(
        //{bold+brwhite}./file:{bold+brblue}{}{bold+brwhite}:{bold+brblue}{}{bold+brwhite}: {bold+red}error: {bold+white}{s}{reset}
        // (b (brwhite "./file:") (brblue "{}") (brwhite ":") (brblue "{}") (brwhite ": ") (red "error: ") (white "{s}"))
        "\x1b[1m\x1b[97m./file:\x1b[94m{}\x1b[97m:\x1b[94m{}\x1b[97m: \x1b[31merror: \x1b[97m{s}\x1b(B\x1b[m\n",
        .{ lyn + 1, col + 1, msg },
    );
    try out.print("{s}\n", .{lineText});
    for (range(unicodeColumnLen(lineText[0..col]))) |_| {
        try out.writeByte(' ');
    }
    try out.writeByte('^');
    try out.writeByte('\n');
    return error.Errored;
}

pub fn mainMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpa.deinit());
    const alloc = &gpa.allocator;

    // 1: parse the file → ast
    var data = Data{
        .err = null,
        .ts = TokenStream.from("{" ++ sample_code ++ "}"), // hack for now; there is no parseTopLevel fn yet
    };
    defer if (data.err) |err| err.deinit();

    const parsed = AstDecl.parse(alloc, &data) catch |e| switch (e) {
        error.ParseError => {
            const err_data = data.err.?;
            return printReportedError(err_data.start, err_data.msg, data.ts.string);
        },
        else => return e,
    };
    defer parsed.deinit(alloc);

    if (true) {
        const stdout = std.io.getStdOut().writer();
        try printAst(parsed, stdout, .{});
        try stdout.writeByte('\n');
    }

    // 2: transform the ast → unallocated ir
    var unallocated = std.ArrayList(IR_Instruction).init(alloc);
    defer {
        for (unallocated.items) |*una| una.deinit();
        unallocated.deinit();
    }

    var irgen_data = IrgenData{
        .err = null,
    };
    var outest_scope = try Scope.newBase(alloc);
    defer outest_scope.destroy();

    irgen(&irgen_data, outest_scope, parsed.value.block.decls, &unallocated) catch |e| switch (e) {
        IrgenError.IrgenError => {
            const err_data = irgen_data.err.?;
            return printReportedError(err_data.src.start, err_data.msg, data.ts.string);
        },
        IrgenError.OutOfMemory => return e,
    };

    // 3: transform the unallocated ir → machine code
}

pub fn main() !u8 {
    mainMain() catch |e| switch (e) {
        error.Errored => return 1,
        else => return e,
    };
    return 0;
}
