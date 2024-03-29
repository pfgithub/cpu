const std = @import("std");
const logic_text = @import("logic");

fn IndexedSlice(comptime T: type, comptime Index: type) type {
    return struct {
        const This = @This();
        slice: []T,
        pub fn init(slice: []T) This {
            return .{ .slice = slice };
        }
        pub fn toOwnedSlice(this: *const This) []T {
            return this.slice;
        }
        pub fn get(this: *const This, index: Index) T {
            return this.slice[@enumToInt(index)];
        }
        pub fn set(this: *This, index: Index, value: T) void {
            this.slice[@enumToInt(index)] = value;
        }
    };
}

// what if instead this was struct{v: u64, u: u64} and an enum was generated from it?
// or alternatively, what if this was handled on the js side
const InputStruct = struct {
    ram_in: u64,
    ram_in_available: u1,
};
fn EnumFromStruct(comptime Struct: type) type {
    var items = [_]std.builtin.TypeInfo.EnumField{undefined} ** @typeInfo(Struct).Struct.fields.len;
    for (@typeInfo(Struct).Struct.fields) |field, i| {
        items[i] = .{ .name = field.name, .value = i };
    }
    return @Type(std.builtin.TypeInfo{
        .Enum = .{
            .layout = .Auto,
            .tag_type = std.math.IntFittingRange(0, items.len),
            .fields = &items,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_exhaustive = true,
        },
    });
}
const InputType = EnumFromStruct(InputStruct);
const OutputStruct = struct {
    ram_out_addr: u61,
    ram_out_set: u1,
    ram_out_set_value: u64,

    r0: u64 = 0,
    r1: u64 = 0,
    r2: u64 = 0,
    r3: u64 = 0,
    r4: u64 = 0,
    r5: u64 = 0,
    r6: u64 = 0,
    r7: u64 = 0,
    r8: u64 = 0,
    r9: u64 = 0,
    rA: u64 = 0,
    rB: u64 = 0,
    rC: u64 = 0,
    rD: u64 = 0,
    rE: u64 = 0,
    pc: u64 = 0,
};
const OutputType = EnumFromStruct(OutputStruct);

const IOSize = u64;
const IOShiftSize = std.math.Log2Int(IOSize);

const InputArray = struct {
    values: [@typeInfo(InputStruct).Struct.fields.len]IOSize,
    // generate a struct type with @Type() {left: IOSize, right: IOSize, add1l: IOSize, add1r: IOSize, …}
    pub fn init(istruct: InputStruct) InputArray {
        return InputArray{
            .values = blk: {
                var values = [_]IOSize{undefined} ** @typeInfo(InputStruct).Struct.fields.len;
                inline for (@typeInfo(InputStruct).Struct.fields) |field, index| {
                    values[index] = @field(istruct, field.name);
                }
                break :blk values;
            },
        };
    }
    pub fn get(iarr: InputArray, field: InputType, index: IOShiftSize) u1 {
        return @intCast(u1, (iarr.values[@enumToInt(field)] >> index) & 0b1);
    }
};
const OutputArray = struct {
    values: [@typeInfo(OutputType).Enum.fields.len]IOSize = [_]IOSize{0} ** @typeInfo(OutputType).Enum.fields.len,
    pub fn set(oarr: *OutputArray, field: OutputType, index: IOShiftSize, value: u1) void {
        oarr.values[@enumToInt(field)] |= @as(IOSize, value) << index;
    }
    pub fn get(oarr: OutputArray, field: OutputType) IOSize {
        return oarr.values[@enumToInt(field)];
    }
    pub fn pack(oarr: OutputArray) OutputStruct {
        var os: OutputStruct = std.mem.zeroes(OutputStruct);
        inline for (@typeInfo(OutputStruct).Struct.fields) |_, i| {
            @field(os, @tagName(@intToEnum(OutputType, i))) = @intCast(
                @TypeOf(@field(os, @tagName(@intToEnum(OutputType, i)))),
                oarr.get(@intToEnum(OutputType, i)),
            );
        }
        return os;
    }
};

const PinsParser = struct {
    pub const Pin = union(enum) {
        in: struct { name: []const u8 },
        out: struct { name: []const u8, dep: usize },

        nor: struct { deps: [2]usize },
        constant: struct { value: u1 },
        state: struct { dep: usize, initial: u1 },
    };

    text: []const u8,

    pub fn from(text: []const u8) PinsParser {
        return PinsParser{ .text = text };
    }

    pub fn next(parser: *PinsParser) ?Pin {
        const line = parser.takeLine() orelse return null;
        var split = std.mem.split(u8, line, "\t");
        defer std.debug.assert(split.next() == null); // error means extra values
        const id = split.next() orelse unreachable; // error means logic.txt is bad.
        std.debug.assert(id.len == 1);
        return switch (id[0]) {
            'i' => Pin{
                .in = .{ .name = split.next() orelse unreachable },
            },
            'o' => Pin{
                .out = .{
                    .name = split.next() orelse unreachable,
                    .dep = (std.fmt.parseInt(usize, split.next() orelse unreachable, 10) catch unreachable) - 1,
                },
            },
            'c' => Pin{
                .constant = .{ .value = std.fmt.parseInt(u1, split.next() orelse unreachable, 2) catch unreachable },
            },
            'n' => Pin{
                .nor = .{
                    .deps = [_]usize{
                        (std.fmt.parseInt(usize, split.next() orelse unreachable, 10) catch unreachable) - 1,
                        (std.fmt.parseInt(usize, split.next() orelse unreachable, 10) catch unreachable) - 1,
                    },
                },
            },
            's' => Pin{
                .state = .{
                    .dep = (std.fmt.parseInt(usize, split.next() orelse unreachable, 10) catch unreachable) - 1,
                    .initial = std.fmt.parseInt(u1, split.next() orelse unreachable, 10) catch unreachable,
                },
            },
            else => unreachable, // bad id
        };
    }
    pub fn takeLine(parser: *PinsParser) ?[]const u8 {
        if (parser.text.len == 0) return null;
        const index = std.mem.indexOf(u8, parser.text, "\n") orelse parser.text.len;

        const until_newline = parser.text[0..index];
        parser.text = std.mem.trimLeft(u8, parser.text[index..], "\r\n");
        return std.mem.trimRight(u8, until_newline, "\r"); // I'm not sure if windows puts \r but just in case
    }
};

const LogicExecutor = struct {
    const ExecutionPin = union(enum) {
        nor: struct { deps: [2]PinIndex },
        constant: struct { value: u1 },
        state: struct { state_id: StateIndex },
        input: struct { input_type: InputType, input_index: IOShiftSize },
        output: struct { output_id: usize },
    };
    const State = struct {
        value: u1,
        next_value: u1,
        dep: PinIndex,
    };
    const Output = struct {
        output_type: OutputType,
        output_index: IOShiftSize,
        dep: PinIndex,
        value: u1,
    };
    const PinIndex = enum(usize) { _ };
    const StateIndex = enum(usize) { _ };

    logic_cache: IndexedSlice(u2, PinIndex), // this needs to be reset to null on every step. a u2 is used here because ?u1 is actually 2 bytes oops
    pins: IndexedSlice(ExecutionPin, PinIndex),
    states: IndexedSlice(State, StateIndex),
    outputs: []Output,
    alloc: *std.mem.Allocator,

    pub fn init(alloc: *std.mem.Allocator, pins: []const u8) !LogicExecutor {
        var execution_pins = try alloc.alloc(ExecutionPin, pins.len);
        errdefer alloc.free(execution_pins);

        var states_al = std.ArrayList(State).init(alloc);
        errdefer states_al.deinit();

        var outputs_al = std.ArrayList(Output).init(alloc);
        errdefer outputs_al.deinit();

        var input_kind_seen_count = std.AutoHashMap(InputType, IOShiftSize).init(alloc);
        defer input_kind_seen_count.deinit();

        var output_kind_seen_count = std.AutoHashMap(OutputType, IOShiftSize).init(alloc);
        defer output_kind_seen_count.deinit();

        var logic_cache = try alloc.alloc(u2, pins.len);
        errdefer alloc.free(logic_cache);

        var i: usize = 0;
        var pins_iter = PinsParser.from(pins);
        while (pins_iter.next()) |pin| : (i += 1) {
            execution_pins[i] = switch (pin) {
                .nor => |nor| .{ .nor = .{ .deps = [_]PinIndex{ @intToEnum(PinIndex, nor.deps[0]), @intToEnum(PinIndex, nor.deps[1]) } } },
                .constant => |constant| .{ .constant = .{ .value = constant.value } },
                .state => |state| blk: {
                    const state_v = State{ .value = state.initial, .next_value = undefined, .dep = @intToEnum(PinIndex, state.dep) };
                    const index = @intToEnum(StateIndex, states_al.items.len);
                    try states_al.append(state_v);
                    break :blk .{ .state = .{ .state_id = index } };
                },
                .in => |in| blk: {
                    const input_type = std.meta.stringToEnum(InputType, in.name) orelse {
                        std.log.emerg("Input named {s} not supported.", .{in.name});
                        return error.BadInputType;
                    };
                    const v = try input_kind_seen_count.getOrPut(input_type);
                    if (!v.found_existing) v.value_ptr.* = 0 else {
                        if (v.value_ptr.* == std.math.maxInt(IOShiftSize)) {
                            std.log.emerg("More than {} inputs named {}", .{ std.math.maxInt(IOShiftSize), input_type });
                            @panic("crash");
                        }
                        v.value_ptr.* += 1;
                    }
                    break :blk .{ .input = .{ .input_type = input_type, .input_index = v.value_ptr.* } };
                },
                .out => |out| blk: {
                    const output_type = std.meta.stringToEnum(OutputType, out.name) orelse {
                        std.log.emerg("Output named {s} not supported.", .{out.name});
                        return error.BadInputType;
                    };
                    const v = try output_kind_seen_count.getOrPut(output_type);
                    if (!v.found_existing) v.value_ptr.* = 0 else {
                        if (v.value_ptr.* == std.math.maxInt(IOShiftSize)) {
                            std.log.emerg("More than {} outputs named {}", .{ std.math.maxInt(IOShiftSize), output_type });
                            @panic("crash");
                        }
                        v.value_ptr.* += 1;
                    }
                    const index = outputs_al.items.len;
                    try outputs_al.append(Output{
                        .output_type = output_type,
                        .output_index = v.value_ptr.*,
                        .dep = @intToEnum(PinIndex, out.dep),
                        .value = undefined,
                    });
                    break :blk .{ .output = .{ .output_id = index } };
                },
            };
        }
        return LogicExecutor{
            .pins = IndexedSlice(ExecutionPin, PinIndex).init(execution_pins),
            .states = IndexedSlice(State, StateIndex).init(states_al.toOwnedSlice()),
            .outputs = outputs_al.toOwnedSlice(),
            .logic_cache = IndexedSlice(u2, PinIndex).init(logic_cache),
            .alloc = alloc,
        };
    }
    pub fn deinit(le: *LogicExecutor) void {
        le.alloc.free(le.pins.slice);
        le.alloc.free(le.states.slice);
        le.alloc.free(le.outputs);
        le.alloc.free(le.logic_cache.slice);
    }

    fn resolve(le: *LogicExecutor, pin_index: PinIndex, inputs: InputArray) u1 {
        // how to do this without recursion:
        // array of dependencies to resolve (or one of those linked list things with array sections or something)
        // every time a pin is found that hasn't been resolved yet, add it and the current pin to the resolution list
        // is this faster? probably not but who knows
        // is this not recursive? yes
        if (le.logic_cache.get(pin_index) & 0b10 == 0) return @intCast(u1, le.logic_cache.get(pin_index));
        const pin = le.pins.get(pin_index);
        const res: u1 = switch (pin) {
            //             nor: struct { deps: [2]PinIndex },
            // constant: struct { value: u1 },
            // state: struct { state_id: StateIndex },
            // input: struct { input_type: InputType, input_index: usize },
            // output: struct { output_id: usize },
            .nor => |nor| for (nor.deps) |dep| {
                // potential optimization here : rather than going left to right, go from lowest depth to highest depth.
                // check caches first in case a cache is available
                if (le.resolve(dep, inputs) == 1) break @as(u1, 0);
            } else 1,
            .constant => |constant| constant.value,
            .state => |state| le.states.get(state.state_id).value,
            .input => |in| inputs.get(in.input_type, in.input_index),
            .output => unreachable, // not supposed to depend on output; bad
        };
        le.logic_cache.set(pin_index, res);
        return res;
    }

    pub fn cycle(le: *LogicExecutor, input_struct: InputStruct) OutputStruct {
        const inputs = InputArray.init(input_struct);
        // 1: clear cache
        for (le.logic_cache.slice) |*item| item.* = 0b10;
        // 2: resolve next states
        for (le.states.slice) |*state| {
            state.next_value = le.resolve(state.dep, inputs);
        }
        // 3: resolve outputs
        for (le.outputs) |*output| {
            output.value = le.resolve(output.dep, inputs);
        }
        // 4: set next states
        for (le.states.slice) |*state| {
            state.value = state.next_value;
            state.next_value = undefined;
        }
        // 5: construct return value
        var outv = OutputArray{};
        for (le.outputs) |output| {
            outv.set(output.output_type, output.output_index, output.value);
        }
        return outv.pack();
    }
};

pub fn updateInputs(outputs: OutputStruct, ram: []u64) InputStruct {
    const ram_v: struct { value: u64, on: u1 } = blk: {
        if (outputs.ram_out_addr != 0) {
            const out_addr = @intCast(usize, outputs.ram_out_addr); // crash: value is outside of ram / usize range
            if (outputs.ram_out_set == 1) {
                ram[out_addr] = outputs.ram_out_set_value; // crash: ram_out_set_value is > 64 bytes
            }
            break :blk .{ .value = ram[out_addr], .on = 1 };
        }
        break :blk .{ .value = 0, .on = 0 };
    };
    return .{
        .ram_in = ram_v.value,
        .ram_in_available = ram_v.on,
    };
}

pub fn range(max: usize) []const void {
    return @as([]const void, &[_]void{}).ptr[0..max];
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());
    const alloc = &gpa.allocator;

    var timer = try std.time.Timer.start();
    timer.reset();

    var executor = try LogicExecutor.init(alloc, logic_text.text);
    defer executor.deinit();

    std.log.info("Executor executed in: {}", .{timer.read()});
    timer.reset();

    var ram = try alloc.alloc(u64, 1000000); // 1mb
    defer alloc.free(ram);
    for (ram) |*it| it.* = 0xAAAAAAAAAAAAAAAA;

    // if (args.len != 2) {
    //     std.log.err("Expected `{s} kernel.bin`", .{args[0]});
    //     return error.Errored;
    // }

    const file = try std.fs.cwd().openFile("src/asm.bin", .{});
    defer file.close();
    const reader = file.reader();

    ram[0x0] = undefined; // ram[0] is invalid and doesn't exist

    var i: usize = 1;
    while (true) : (i += 1) {
        const instr = reader.readIntLittle(u64) catch |e| switch (e) {
            error.EndOfStream => break,
            else => return e,
        };
        ram[i] = instr;
    }

    std.log.info("Ram initialized in: {}", .{timer.read()});

    var inputs = updateInputs(OutputStruct{
        .ram_out_addr = 0,
        .ram_out_set = 0,
        .ram_out_set_value = 0,
    }, ram);

    timer.reset();

    for (range(30)) |_| {
        const res = executor.cycle(inputs);
        std.log.info("{X}: r0: {X}, r1: {X}, r2: {X}, r3: {X}, ram_set_v: {X}", .{ res.pc, @bitCast(i64, res.r0), @bitCast(i64, res.r1), res.r2, res.r3, res.ram_out_set_value });
        inputs = updateInputs(res, ram);
    }

    std.log.info("Took: {}", .{timer.read()});
}
