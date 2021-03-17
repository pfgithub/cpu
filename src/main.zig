const std = @import("std");
const logic = @import("logic");

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

const InputType = enum {
    left,
    right,
    add1l,
    add1r,
    add1c,
};
const OutputType = enum {
    total,
    added1,
    counter,
};

fn EnumStruct(comptime Enum: type, comptime Value: type) type {
    var field_list = [_]std.builtin.TypeInfo.StructField{undefined} ** @typeInfo(Enum).Enum.fields.len;
    // a comptime map would be useful here
    const enum_fields: []const std.builtin.TypeInfo.EnumField = @typeInfo(Enum).Enum.fields;
    for (enum_fields) |field, i| {
        field_list[i] = .{ .name = field.name, .field_type = Value, .default_value = null, .is_comptime = false, .alignment = 0 };
    }
    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &field_list,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_tuple = false,
        },
    });
}

const InputArray = struct {
    values: [@typeInfo(InputType).Enum.fields.len]u64,
    // generate a struct type with @Type() {left: u64, right: u64, add1l: u64, add1r: u64, …}
    const InitializationStruct = EnumStruct(InputType, u64);
    pub fn init(istruct: InitializationStruct) InputArray {
        return InputArray{
            .values = blk: {
                var values = [_]u64{undefined} ** @typeInfo(InputType).Enum.fields.len;
                inline for (@typeInfo(InputType).Enum.fields) |field, index| {
                    if (field.value != index) @compileError("bad");
                    values[field.value] = @field(istruct, field.name);
                }
                break :blk values;
            },
        };
    }
    pub fn get(iarr: InputArray, field: InputType, index: u6) u1 {
        return @intCast(u1, (iarr.values[@enumToInt(field)] >> index) & 0b1);
    }
};

const LogicExecutor = struct {
    const ExecutionPin = union(enum) {
        nor: struct { deps: [2]PinIndex },
        constant: struct { value: u1 },
        state: struct { state_id: StateIndex },
        input: struct { input_type: InputType, input_index: u6 },
        output: struct { output_id: usize }, // output_index, output_offset
    };
    const State = struct {
        value: u1,
        next_value: u1,
        dep: PinIndex,
    };
    const Output = struct {
        output_type: OutputType,
        dep: PinIndex,
        value: usize,
    };
    const PinIndex = enum(usize) { _ };
    const StateIndex = enum(usize) { _ };

    logic_cache: IndexedSlice(u2, PinIndex), // this needs to be reset to null on every step. a u2 is used here because ?u1 is actually 2 bytes oops
    pins: IndexedSlice(ExecutionPin, PinIndex),
    states: IndexedSlice(State, StateIndex),
    outputs: []Output,
    alloc: *std.mem.Allocator,

    pub fn init(alloc: *std.mem.Allocator, pins: []const logic.Pin) !LogicExecutor {
        var execution_pins = try alloc.alloc(ExecutionPin, pins.len);
        errdefer alloc.free(execution_pins);

        var states_al = try std.ArrayList(State).initCapacity(alloc, len: {
            var len: usize = 0;
            for (pins) |pin| if (pin == .out) {
                len += 1;
            };
            break :len len;
        });
        errdefer states_al.deinit();

        var outputs_al = try std.ArrayList(Output).initCapacity(alloc, len: {
            var len: usize = 0;
            for (pins) |pin| if (pin == .out) {
                len += 1;
            };
            break :len len;
        });
        errdefer outputs_al.deinit();

        var input_kind_seen_count = std.AutoHashMap(InputType, u6).init(alloc);
        defer input_kind_seen_count.deinit();

        var logic_cache = try alloc.alloc(u2, pins.len);
        errdefer alloc.free(logic_cache);

        for (pins) |pin, i| {
            execution_pins[i] = switch (pin) {
                .nor => |nor| .{ .nor = .{ .deps = [_]PinIndex{ @intToEnum(PinIndex, nor.deps[0]), @intToEnum(PinIndex, nor.deps[1]) } } },
                .constant => |constant| .{ .constant = .{ .value = constant.value } },
                .state => |state| blk: {
                    const state_v = State{ .value = state.initial, .next_value = undefined, .dep = @intToEnum(PinIndex, state.dep) };
                    const index = @intToEnum(StateIndex, states_al.items.len);
                    states_al.appendAssumeCapacity(state_v);
                    break :blk .{ .state = .{ .state_id = index } };
                },
                .in => |in| blk: {
                    const input_type = std.meta.stringToEnum(InputType, in.name) orelse @panic("bad input type");
                    const v = try input_kind_seen_count.getOrPut(input_type);
                    if (!v.found_existing) v.entry.value = 0 else {
                        if (v.entry.value == std.math.maxInt(u6)) {
                            std.log.emerg("More than {} inputs named {}", .{ std.math.maxInt(u6), input_type });
                            @panic("crash");
                        }
                        v.entry.value += 1;
                    }
                    break :blk .{ .input = .{ .input_type = input_type, .input_index = v.entry.value } };
                },
                .out => |out| blk: {
                    const output_type = std.meta.stringToEnum(OutputType, out.name) orelse @panic("bad output type");
                    const index = outputs_al.items.len;
                    outputs_al.appendAssumeCapacity(Output{ .output_type = output_type, .dep = @intToEnum(PinIndex, out.dep), .value = undefined });
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

    pub fn cycle(le: *LogicExecutor, inputs: InputArray) void {
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
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpa.deinit());
    const alloc = &gpa.allocator;

    var executor = try LogicExecutor.init(alloc, logic.pins);
    defer executor.deinit();

    const inputs = InputArray.init(.{
        .left = 0b1,
        .right = 0b1,
        .add1l = 0b1,
        .add1r = 0b1,
        .add1c = 0b1,
    });

    const timer = try std.time.Timer.start();

    executor.cycle(inputs);
    executor.cycle(inputs);
    executor.cycle(inputs);
    executor.cycle(inputs);
    executor.cycle(inputs);
    executor.cycle(inputs);

    const end = timer.read();
    for (executor.outputs) |output| {
        std.log.info("{}: {}", .{ output.output_type, output.value });
    }
    std.log.info("Took: {}", .{end});
}
