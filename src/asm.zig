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
                            errdefer expr.deinit(alloc);
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
const OutVarType = union(enum) {
    variable: []const u8,
    reg: []const u8,
    // maybe this should just be *AstExpr
};
const AstDecl = struct {
    value: union(enum) {
        exec: struct {
            out_var: ?OutVarType,
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
        //   #reg = expression
        //   label:
        //   { …decl[] }
        const ts = &data.ts;

        eatWhitespace(ts);
        const start_index = ts.index;
        errdefer ts.index = start_index;

        const swhash = ts.startsWithTake("#");
        const id_opt = readID(ts);
        if (id_opt == null) ts.index = start_index;

        eatWhitespace(ts);
        if (id_opt) |id| continu: {
            // this type of stuff is what a tokenizer helps simplify
            // this eventually will need to parse `label:`, `label:=`, `label=`, `label :=`, `label =`, but not `label :`
            // a tokenizer could emit (lbl) (id)(:=) (id)(=) (id)(:=) (id)(=) (id)(error)
            if (ts.startsWithTake(":")) {
                const end_index = ts.index;
                eatWhitespace(ts);
                return AstDecl{
                    .src = .{ .start = start_index, .end = end_index },
                    .value = .{ .label = .{ .name = id } },
                };
            }
            eatWhitespace(ts);
            if (ts.startsWithTake("=")) {
                eatWhitespace(ts);
                break :continu;
            }
            return parseError(data, ts.index, "expected `=` or `:`");
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
            '(' => {
                // continue
            },
            else => return parseError(data, ts.index, "expected (instruction …args)"),
        }

        var slot = try alloc.create(AstExpr);
        errdefer alloc.destroy(slot);

        slot.* = try AstExpr.parse(alloc, data);
        errdefer slot.deinit(alloc);
        eatWhitespace(ts);

        return AstDecl{
            .src = .{ .start = start_index, .end = slot.src.end },
            .value = .{
                .exec = .{
                    .out_var = if (id_opt) |io| if (swhash) OutVarType{ .reg = io } else OutVarType{ .variable = io } else null,
                    .expr = slot,
                },
            },
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
            if (exc.out_var) |ov| switch (ov) {
                .variable => |vbl| try out.print("{s} = ", .{vbl}),
                .reg => |reg| try out.print("#{s} = ", .{reg}),
            };
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
const Arg = struct {
    value: union(enum) {
        register: VariableDefinition,
        immediate: ImmediateValue,
        out_reg: VariableDefinition,
        cleared_regs_bitfield: u15,
        raw_reg: SystemRegister,
        immediate_target: ImmediateValue,
    },
    src: Src,
};

const IR_Block = struct {
    instructions: []IR_Instruction,
};

const Src = struct { start: usize, end: usize };

const SystemRegister = enum(u4) {
    r0,
    t0, // not preserved across calls
    t1, // also used as function arguments and return values
    t2,
    t3,
    t4,
    t5,
    s0, // preserved across calls
    s1,
    s2,
    s3,
    s4,
    s5,
    ra, // return address
    sp, // stack pointer
    pc,
    // huh this isn't very many registers
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
    out_block: *std.ArrayList(IR_Instruction),
    labels: *std.AutoHashMap(usize, IrgenLabel), // label id → out_block index
};
const IrgenLabel = struct {
    res: usize,
    src: Src,
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

pub fn irgen(data: *IrgenData, parent_scope: *Scope, outer_block: []AstDecl) IrgenError!void {
    const scope = try Scope.new(parent_scope);
    defer scope.destroy();

    const out_block = data.out_block;

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
                break :blk switch (ovname) {
                    .variable => |vbl| break :blk scope.getVariable(vbl) orelse {
                        const vardef = VariableDefinition{
                            .value = .{
                                .unallocated = .{
                                    .definition_src = decl.src, // TODO exec.out_var_src?
                                    .id = nextID(),
                                },
                            },
                            .space = .normal,
                        };
                        try scope.defVariable(vbl, vardef);
                        break :blk vardef;
                    },
                    .reg => |rg| VariableDefinition{
                        .space = .normal,
                        .value = .{
                            .allocated = std.meta.stringToEnum(SystemRegister, rg) orelse {
                                // TODO exec.out_var_src?
                                return irgenError(data, decl.src, "No register with this name exists");
                            },
                        },
                    },
                };
            };

            try irgenReg(data, scope, out_var, exec.expr.*);
        },
        .block => |block| {
            try irgen(data, scope, block.decls);
        },
        .label => |lbl| {
            const label: LabelDefinition = scope.getLabel(lbl.name) orelse unreachable;
            try data.labels.putNoClobber(label.id, .{ .res = out_block.items.len, .src = decl.src });
        },
    };
}

const IR_Instruction = struct {
    value: union(enum) {
        standard_instr: struct {
            instr_id: InstructionID,
            args: [InstructionMaxArgsCount]Arg,
            next: CfMode,
        },
        raw_value: u64,
    },
    src: Src,
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

// zig fmt: off
const MultilineInstructionsHack = struct {
    usingnamespace InstrInfo;
    pub const Instructions = .{
        // register allocator doesn't care where the target is.
        // for the purpose of register allocator control flow, this
        // instruction is completely normal other than clearing a few
        // registers after being called
        .{ "call", instr(.jal, &[_]ArgSpec{
            constant(u4, @enumToInt(SystemRegister.pc)), // base
            rawReg("ret_addr", .normal), // ←ret_addr // this is *not* an out register. it's not `#ra = (call …)`.
            immediate("jump_target", i48), // offset
            savedRegsBitfield("saved_regs", .normal),
        }, .next) },

        // register allocator follows this control flow
        .{ "jump", instr(.jal, &[_]ArgSpec{
            constant(u4, @enumToInt(SystemRegister.pc)), // base
            constant(u4, @enumToInt(SystemRegister.pc)), // ←ret_addr (voided)
            immediateTarget("jump_target", i48) // offset
        }, .target), },

        // for the purposes of register allocation, this is equivalent to "halt"
        .{ "ret", instr(.jal, &[_]ArgSpec{
            reg("jump_target", .normal), // base
            constant(u4, @enumToInt(SystemRegister.pc)), // ←ret_addr (voided)
            constant(i48, 0) // offset
        }, .none) },
        
        // "someconditionaljump" (targetImmediate(…)), .either
    };
};
// zig fmt: on

pub const CfMode = enum {
    next, // execution continues to next instruction
    target, // execution continues to targetImmediate
    either, // execution continues to either target or next
    none, // execution halts
};

const InstrInfo = struct {
    pub const instructions = std.ComptimeStringMap(InstructionSpec, .{
        .{ "noop", instr(.noop, &[_]ArgSpec{constant(u56, 0)}, .next) },
        .{ "li", instr(.li, &[_]ArgSpec{ out(.normal), immediate("value", i52) }, .next) },
        .{ "add", instr(.add, &[_]ArgSpec{ reg("lhs", .normal), reg("rhs", .normal), out(.normal), constant(u44, 0) }, .next) },
        .{ "load", instr(.load, &[_]ArgSpec{ reg("addr", .normal), out(.normal), constant(u48, 0) }, .next) },
        .{ "store", instr(.store, &[_]ArgSpec{ reg("addr", .normal), reg("value", .normal), constant(u48, 0) }, .next) },
        .{ "halt", instr(.halt, &[_]ArgSpec{constant(u56, 0)}, .none) },
    } ++ MultilineInstructionsHack.Instructions);

    pub const ArgSpec = union(enum) {
        constant: struct { width: std.math.Log2Int(u64), value: u64 },
        // holds a 4-bit register or register-allocated variable
        reg: struct { name: []const u8, space: RegisterSpace },
        // holds a 4-bit register
        raw_reg: struct { name: []const u8, space: RegisterSpace },
        // holds a 4-bit register or register-allocated variable
        out: struct { space: RegisterSpace },
        // holds a width-bit immediate value
        immediate: struct { name: []const u8, width: std.math.Log2Int(u64), signed: bool },
        // holds a width-bit immediate value that represents where this instruction will continue if CdMode == .target
        immediate_target: struct { name: []const u8, width: std.math.Log2Int(u64), signed: bool },
        // holds information for the register allocator about what registers to mark as cleared after this instruction finishes executing
        saved_regs_bitfield: struct { name: []const u8, space: RegisterSpace },
    };
    pub const InstructionSpec = struct {
        instr_id: InstructionID,
        args: [InstructionMaxArgsCount]ArgSpec,
        cf_mode: CfMode,
    };
    pub fn constant(comptime Width: type, value: Width) ArgSpec {
        const ti: std.builtin.TypeInfo.Int = @typeInfo(Width).Int;
        const bit_count = ti.bits;
        return ArgSpec{
            .constant = .{
                .width = bit_count,
                .value = @bitCast(std.meta.Int(.unsigned, bit_count), value),
            },
        };
    }
    pub fn out(space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .out = .{ .space = space },
        };
    }
    pub fn immediate(name: []const u8, comptime Width: type) ArgSpec {
        const ti: std.builtin.TypeInfo.Int = @typeInfo(Width).Int;
        return ArgSpec{
            .immediate = .{ .name = name, .width = std.meta.bitCount(Width), .signed = ti.signedness == .signed },
        };
    }
    pub fn immediateTarget(name: []const u8, comptime Width: type) ArgSpec {
        const ti: std.builtin.TypeInfo.Int = @typeInfo(Width).Int;
        return ArgSpec{
            .immediate_target = .{ .name = name, .width = std.meta.bitCount(Width), .signed = ti.signedness == .signed },
        };
    }
    pub fn reg(name: []const u8, space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .reg = .{ .name = name, .space = space },
        };
    }
    pub fn rawReg(name: []const u8, space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .raw_reg = .{ .name = name, .space = space },
        };
    }
    pub fn savedRegsBitfield(name: []const u8, space: RegisterSpace) ArgSpec {
        return ArgSpec{
            .saved_regs_bitfield = .{ .name = name, .space = space },
        };
    }
    pub fn instr(comptime id: InstructionID, comptime args: []const ArgSpec, comptime cf_mode: CfMode) InstructionSpec {
        comptime {
            var res_spec: []const ArgSpec = args;
            if (res_spec.len > InstructionMaxArgsCount) @compileError("Too many spec items. Max is InstructionMaxArgsCount");

            var bit_count: comptime_int = 8;
            for (res_spec) |itm| {
                bit_count += @as(comptime_int, switch (itm) {
                    .constant => |cns| cns.width,
                    .reg => 4,
                    .raw_reg => 4,
                    .out => 4,
                    .immediate => |imm| imm.width,
                    .immediate_target => |imm| imm.width,
                    .saved_regs_bitfield => 0,
                });
            }
            if (bit_count != 64) @compileError("Does not add up to 64 bits.");

            while (res_spec.len < InstructionMaxArgsCount) {
                res_spec = res_spec ++ &[_]ArgSpec{constant(u0, 0)};
            }

            return InstructionSpec{
                .instr_id = id,
                .args = res_spec[0..InstructionMaxArgsCount].*,
                .cf_mode = cf_mode,
            };
        }
    }
};

pub fn irgenSysReg(data: *IrgenData, scope: *Scope, expr: AstExpr) IrgenError!SystemRegister {
    const out_block = data.out_block;

    switch (expr.value) {
        .reg => |rg| {
            return std.meta.stringToEnum(SystemRegister, rg.name) orelse {
                return irgenError(data, expr.src, "No register with this name exists");
            };
        },
        else => {
            return irgenError(data, expr.src, "This slot is only for explicit registers eg #pc");
        },
    }
}

pub fn irgenIntermediate(data: *IrgenData, scope: *Scope, space: RegisterSpace, expr: AstExpr) IrgenError!VariableDefinition {
    const out_block = data.out_block;

    switch (expr.value) {
        .variable => |vbl| {
            return scope.getVariable(vbl.name) orelse {
                return irgenError(data, expr.src, "Variable not found");
            };
        },
        .reg => |rg| {
            // this space thing isn't very well thought out because I'm not actually using it
            // I think 'scope' has to be a property of the SystemRegister or something idk
            if (space != .normal) return irgenError(data, expr.src, "Trying to fit non-normal register into normal slot");
            return VariableDefinition{
                .value = .{
                    .allocated = try irgenSysReg(data, scope, expr),
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
            try irgenReg(data, scope, slot, expr);
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

pub fn irgenReg(data: *IrgenData, scope: *Scope, out_reg: ?VariableDefinition, expr: AstExpr) IrgenError!void {
    const out_block = data.out_block;

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
                        .value = .{
                            .immediate = .{
                                .width = cns.width,
                                .value = .{
                                    .constant = cns.value,
                                },
                            },
                        },
                        .src = expr.src,
                    },
                    .reg => |reg| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .value = .{
                                .register = try irgenIntermediate(data, scope, reg.space, arg),
                            },
                            .src = arg.src,
                        };
                    },
                    .out => |out| blk: {
                        const oreg = out_reg orelse {
                            return irgenError(data, expr.src, "Return value is ignored");
                        };
                        break :blk Arg{
                            .value = .{
                                .out_reg = oreg,
                            },
                            .src = expr.src, // TODO fix
                        };
                    },
                    .immediate => |imm| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .value = .{
                                .immediate = try irgenImmediate(data, scope, imm.width, imm.signed, arg),
                            },
                            .src = arg.src,
                        };
                    },
                    .raw_reg => |rreg| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .value = .{
                                .raw_reg = try irgenSysReg(data, scope, arg),
                            },
                            .src = arg.src,
                        };
                    },
                    .immediate_target => |imm| blk: {
                        const arg = args.next() orelse {
                            return irgenError(data, expr.src, "Not enough arguments.");
                        };
                        break :blk Arg{
                            .value = .{
                                .immediate_target = try irgenImmediate(data, scope, imm.width, imm.signed, arg),
                            },
                            .src = arg.src,
                        };
                    },
                    .saved_regs_bitfield => |srbf| blk: {
                        var bitfield_value: u15 = 0;
                        while (args.next()) |arg| {
                            const sys_reg = try irgenSysReg(data, scope, arg);
                            if (sys_reg == .pc) return irgenError(data, arg.src, "pc does not need to be listed here");
                            var v: u15 = @as(u15, 1) << @enumToInt(sys_reg);
                            bitfield_value |= v;
                        }
                        break :blk Arg{
                            .value = .{
                                .cleared_regs_bitfield = ~bitfield_value,
                            },
                            .src = expr.src, // TODO fix
                        };
                        // huh this is an Arg but maybe it should be a property of the instruction
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
                        .next = spec.cf_mode,
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
pub fn irgenImmediate(data: *IrgenData, scope: *Scope, width: std.math.Log2Int(u64), signed: bool, expr: AstExpr) IrgenError!ImmediateValue {
    const out_block = data.out_block;

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

pub fn printIrSysReg(reg: SystemRegister, out: anytype) @TypeOf(out).Error!void {
    try out.writeAll("#");
    try out.writeAll(std.meta.tagName(reg));
}
pub fn printIrReg(reg: VariableDefinition, out: anytype) @TypeOf(out).Error!void {
    switch (reg.value) {
        .allocated => |acd| {
            try printIrSysReg(acd, out);
        },
        .unallocated => |una| {
            try out.print("%{d}", .{una.id});
        },
    }
    // value: union(enum) {
    //     allocated: SystemRegister,
    //     unallocated: struct {
    //         definition_src: Src, // where the variable was defined
    //         id: usize, // a unique id that represents this variable
    //     },
    // },
    // space: RegisterSpace,
}
pub fn printIrArg(item: Arg, out: anytype) @TypeOf(out).Error!void {
    switch (item.value) {
        .register => |reg| {
            try out.writeAll(" ");
            try printIrReg(reg, out);
        },
        .immediate, .immediate_target => |imm| {
            if (imm.width == 0) return;
            switch (imm.value) {
                .constant => |cons| try out.print(" 0x{x}", .{cons}),
                .label => |lbl| try out.print(" :{}", .{lbl.id}),
            }
        },
        .raw_reg => |reg| {
            try out.writeAll(" ");
            try printIrSysReg(reg, out);
        },
        .out_reg => |oreg| {
            try out.writeAll(" ←");
            try printIrReg(oreg, out);
        },
        .cleared_regs_bitfield => |crbf| {
            try out.print(" <clr {b:15}>", .{crbf});
        },
    }
}

pub fn printIrLine(item: IR_Instruction, out: anytype) @TypeOf(out).Error!void {
    // TODO print labels
    switch (item.value) {
        .standard_instr => |instr| {
            const nextname = switch (instr.next) {
                .none => "⎋",
                .target => "→",
                .next => "↓",
                .either => "↘",
            };
            try out.print("{s} {s}", .{ nextname, std.meta.tagName(instr.instr_id) });
            for (instr.args) |arg| {
                try printIrArg(arg, out);
            }
            try out.writeByte('\n');
        },
        .raw_value => |raval| {
            try out.print("{b}\n", .{raval});
        },
    }
}

pub fn resolveLabels(data: *IrgenData) !void {
    for (data.out_block.items) |*instr, index| {
        const index_i64 = @bitCast(i64, index);
        switch (instr.value) {
            .standard_instr => |*si| {
                for (si.args) |*arg| switch (arg.value) {
                    .immediate, .immediate_target => |*imm| switch (imm.value) {
                        .label => |*lbl| {
                            // make sure it fits in width
                            const target_line = @bitCast(i64, data.labels.get(lbl.id).?.res);

                            const offset = runtimeBitcast(target_line - index_i64, imm.width, true) orelse {
                                return irgenError(data, instr.src, "Target label is too far away."); // TODO arg.src
                            };
                            imm.value = .{ .constant = offset };
                        },
                        else => {},
                    },
                    else => {},
                };
            },
            else => {},
        }
    }
}

pub fn codegenError(err: *?IrgenData.Error, src: Src, msg: []const u8) error{CodegenError} {
    err.* = .{ .src = src, .msg = msg };
    return error.CodegenError;
}

pub fn codegen(item: IR_Instruction, err: *?IrgenData.Error) !u64 {
    switch (item.value) {
        .raw_value => |rv| return rv,
        .standard_instr => |sinstr| {
            var res: u64 = @enumToInt(sinstr.instr_id);
            var offset: std.math.Log2Int(u64) = 8;
            for (sinstr.args) |arg| switch (arg.value) {
                .register => |reg| switch (reg.value) {
                    .allocated => |areg| {
                        res |= @as(u64, @enumToInt(areg)) << offset;
                        offset += std.meta.bitCount(std.meta.TagType(@TypeOf(areg)));
                    },
                    .unallocated => {
                        // TODO error at the arg
                        return codegenError(err, item.src, "Register allocation is not supported (yet)");
                    },
                },
                .immediate => |imm| {
                    res |= @as(u64, imm.value.constant) << offset;
                    offset += imm.width;
                },
                .out_reg => |reg| switch (reg.value) {
                    .allocated => |areg| {
                        res |= @as(u64, @enumToInt(areg)) << offset;
                        offset += std.meta.bitCount(std.meta.TagType(@TypeOf(areg)));
                    },
                    .unallocated => {
                        // TODO error at the arg
                        return codegenError(err, item.src, "Register allocation is not supported (yet)");
                    },
                },
                .cleared_regs_bitfield => {},
                .raw_reg => |areg| {
                    res |= @as(u64, @enumToInt(areg)) << offset;
                    offset += std.meta.bitCount(std.meta.TagType(@TypeOf(areg)));
                },
                // this should be merged with .immediate using inline switch once it's released
                .immediate_target => |imm| {
                    res |= @as(u64, imm.value.constant) << offset;
                    offset += imm.width;
                },
            };
            if (offset != 64) unreachable;
        },
    }
    return 0;
}

pub fn mainMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpa.deinit());
    const alloc = &gpa.allocator;

    // 0: read the file
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        std.log.err("Expected `{s} file.asm`", .{args[0]});
        return error.Errored;
    }

    const file_cont = try std.fs.cwd().readFileAlloc(alloc, args[1], std.math.maxInt(usize));
    defer alloc.free(file_cont);

    // 1: parse the file → ast
    var data = Data{
        .err = null,
        .ts = TokenStream.from(file_cont), // hack for now; there is no parseTopLevel fn yet
    };
    defer if (data.err) |err| err.deinit();

    var parsed_al = std.ArrayList(AstDecl).init(alloc);
    defer {
        for (parsed_al.items) |*it| it.deinit(alloc);
        parsed_al.deinit();
    }
    while (data.ts.peek() != 0) {
        try parsed_al.append(AstDecl.parse(alloc, &data) catch |e| switch (e) {
            error.ParseError => {
                const err_data = data.err.?;
                return printReportedError(err_data.start, err_data.msg, data.ts.string);
            },
            else => return e,
        });
    }

    if (true) {
        const stdout = std.io.getStdOut().writer();
        try stdout.writeAll("// AST:\n");
        for (parsed_al.items) |parsed| {
            try printAst(parsed, stdout, .{});
            try stdout.writeByte('\n');
        }
        try stdout.writeAll("\n");
    }

    // 2: transform the ast → unallocated ir
    var unallocated = std.ArrayList(IR_Instruction).init(alloc);
    defer unallocated.deinit();

    var labels = std.AutoHashMap(usize, IrgenLabel).init(alloc);
    defer labels.deinit();

    var irgen_data = IrgenData{
        .err = null,
        .out_block = &unallocated,
        .labels = &labels,
    };
    var outest_scope = try Scope.newBase(alloc);
    defer outest_scope.destroy();

    irgen(&irgen_data, outest_scope, parsed_al.items) catch |e| switch (e) {
        IrgenError.IrgenError => {
            const err_data = irgen_data.err.?;
            return printReportedError(err_data.src.start, err_data.msg, file_cont);
        },
        IrgenError.OutOfMemory => return e,
    };

    resolveLabels(&irgen_data) catch |e| switch (e) {
        IrgenError.IrgenError => {
            const err_data = irgen_data.err.?;
            return printReportedError(err_data.src.start, err_data.msg, file_cont);
        },
        IrgenError.OutOfMemory => return e,
    };

    if (true) {
        const stdout = std.io.getStdOut().writer();
        try stdout.writeAll("// IR:\n");
        for (unallocated.items) |item| try printIrLine(item, stdout);
        try stdout.writeAll("\n");
    }

    // 3: transform the unallocated ir → machine code
    var res_code = std.ArrayList(u64).init(alloc);
    defer res_code.deinit();

    var codegen_error: ?IrgenData.Error = null;

    for (unallocated.items) |instr| {
        const value = codegen(instr, &codegen_error) catch |e| switch (e) {
            error.CodegenError => return printReportedError(codegen_error.?.src.start, codegen_error.?.msg, file_cont),
        };
        try res_code.append(value);
    }
}

pub fn main() !u8 {
    mainMain() catch |e| switch (e) {
        error.Errored => return 1,
        else => return e,
    };
    return 0;
}
