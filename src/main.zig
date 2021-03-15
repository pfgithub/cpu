const std = @import("std");

const Pin = struct {
    value: union(enum) {
        not: struct {
            dependency: *Pin,
        },
        nor: struct {
            dependencies: [2]*Pin,
        },
        input: struct {
            value: *u1,
        },
        output: struct {
            dependency: *Pin,
        },
    },

    fn getValue(pin: *Pin) u1 {
        switch (pin.value) {
            .not => |not| {
                return @boolToInt(not.dependency.getValue() == 0);
            },
            .nor => |nor| {
                for (nor.dependencies) |dependency| {
                    if (dependency.getValue() == 1) return 0;
                }
                return 1;
            },
            .input => |in| {
                return in.value.*;
            },
            .output => |out| {
                return out.dependency.getValue();
            },
        }
    }

    fn newInput(alloc: *std.mem.Allocator, value: *u1) *Pin {
        const pin = alloc.create(Pin) catch @panic("oom");
        pin.* = .{
            .value = .{ .input = .{ .value = value } },
        };
        return pin;
    }
    fn newOutput(alloc: *std.mem.Allocator, dep: *Pin) *Pin {
        const pin = alloc.create(Pin) catch @panic("oom");
        pin.* = .{
            .value = .{ .output = .{ .dependency = dep } },
        };
        return pin;
    }
    fn newNor(alloc: *std.mem.Allocator, lhs: *Pin, rhs: *Pin) *Pin {
        const pin = alloc.create(Pin) catch @panic("oom");
        pin.* = .{
            .value = .{ .nor = .{ .dependencies = &[_]*Pin{ lhs, rhs } } },
        };
        return pin;
    }
    fn newNot(alloc: *std.mem.Allocator, dep: *Pin) *Pin {
        const pin = alloc.create(Pin) catch @panic("oom");
        pin.* = .{
            .value = .{ .not = .{ .dependency = dep } },
        };
        return pin;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = &arena.allocator;

    var timer: u1 = 0;
    const timerPin = Pin.newInput(alloc, &timer);
    const inverter = Pin.newNot(alloc, timerPin);
    const setTimerPin = Pin.newOutput(alloc, inverter);

    // demo:
    std.log.info("timer: {}", .{timer});
    timer = setTimerPin.getValue();
    std.log.info("timer: {}", .{timer});
    timer = setTimerPin.getValue();
    std.log.info("timer: {}", .{timer});
}
