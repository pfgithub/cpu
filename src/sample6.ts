type Tuple<T, N extends number> = N extends 64
    ? [
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
    ]
    : N extends 61
    ? [
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
    ]
    : N extends N ? number extends N ? T[] : _TupleOf<T, N, []> : never;
type _TupleOf<T, N extends number, R extends unknown[]> = R['length'] extends N ? R : _TupleOf<T, N, [T, ...R]>;

type Pins<L extends number> = Tuple<Pin, L>;
type Pin = {__opaque_is_pin: true};
type PinData =
    | {kind: "in", name: string}
    | {kind: "out", name: string, dep: Pin}
    | {kind: "state", dep: Pin | undefined, initial: 0 | 1, debug: string}
    | {kind: "nor", deps: [Pin, Pin]}
    | {kind: "const", value: 0 | 1}
;
type StateItem = {value: Pin, setValue: (dep: Pin) => void};
const builtin = {
    pins: [] as PinData[],
    pin_cache: new Map<string, Pin>(),
    getPin(pin: Pin): PinData {
        return builtin.pins[(pin as unknown as number) - 1]!;
    },
    addPin(data: PinData): Pin {
        const stringified = JSON.stringify(data);
        const cached = builtin.pin_cache.get(stringified);
        if(data.kind !== "in" && data.kind !== "out" && data.kind !== "state" && cached) return cached;
        const pin = builtin.pins.push(data) as unknown as Pin;
        // note this must be 1-indexed otherwise pin might not always be truthy when it exists
        // and that is not properly reflected in the type system.
        builtin.pin_cache.set(stringified, pin);
        return pin;
    },
    in<Length extends number>(name: string, count: Length): Tuple<Pin, Length> {
        const res: Pin[] = [];
        for(let i = 0; i < count; i++) {
            res.push(builtin.addPin({kind: "in", name}));
        }
        return res as unknown as Tuple<Pin, Length>;
    },
    nor(a: Pin, b: Pin): Pin {
        const pa = builtin.getPin(a);
        const pb = builtin.getPin(b);
        if(pa.kind === "const" && pa.value === 1) return builtin.addPin({kind: "const", value: 0});
        if(pb.kind === "const" && pb.value === 1) return builtin.addPin({kind: "const", value: 0});
        if(pa.kind === "const" && pb.kind === "const") return builtin.addPin({kind: "const", value: 1});
        return builtin.addPin({kind: "nor", deps: [a, b]});
    },
    out(name: string, pins: Pin[]) {
        pins.forEach(pin =>
            builtin.addPin({kind: "out", dep: pin, name})
        );
    },
    const(value: 0 | 1): Pin {
        return builtin.addPin({kind: "const", value});
    },
    constw<Length extends number>(w: Length, initial: string): Pins<Length> {
        return new Array(w).fill(0).map((_, i) => {
            return builtin.const(initial.charAt(initial.length - i - 1) === "1" ? 1 : 0);
        }) as Pins<Length>;
    },
    state<Length extends number>(width: Length, initial: string = ""): {set: (value: Pins<Length>) => void, value: Pins<Length>} {
        const res: StateItem[] = new Array(width).fill(0).map((_, i): StateItem => {
            const pin_data: PinData = {kind: "state", dep: undefined, initial: initial.charAt(initial.length - i - 1) === "1" ? 1 : 0, debug: new Error().stack ?? ""};
            return {value: builtin.addPin(pin_data), setValue: (dep) => {
                if(pin_data.dep) throw new Error("dep already set");
                pin_data.dep = dep;
            }};
        });
        return {
            value: res.map(r => r.value) as Pins<Length>,
            set: (value) => {
                res.forEach((item, i) => item.setValue(value[i]!));
            },
        };
    }
};

function nor(...all: Pin[]): Pin {
    if(all.length >= 2) {
        const [a, b, ...rest] = all as [Pin, Pin, ...Pin[]];
        return rest.reduce((t, c) => builtin.nor(not(t), c), builtin.nor(a, b));
    }else if(all.length === 1) {
        return not(all[0]!);
    }else{
        throw new Error("cannot nor 0 values");
    }
}
// TODO tests like
// test {
//    nor(...builtin.constw(3, "010")) === 0
//    nor(...builtin.constw(3, "000")) === 1
//    nor(...builtin.constw(3, "001")) === 0
//    …
// }
function not(a: Pin): Pin {
    return builtin.nor(a, a);
}
function and(a: Pin, b: Pin, ...rest: Pin[]): Pin {
    return rest.reduce((t, c) => and(t, c),
        nor(not(a), not(b))
    );
}
function or(a: Pin, b: Pin, ...rest: Pin[]): Pin {
    return rest.reduce((t, c) => or(t, c),
        not(nor(a, b))
    );
}
function xor(a: Pin, b: Pin, ...rest: Pin[]): Pin {
    return rest.reduce((t, c) => xor(t, c), nor(
        and(a, b),
        nor(a, b),
    ));
}
function halfAdder(l: Pin, r: Pin): {sum: Pin, carry: Pin} {
    return {
        sum: xor(l, r),
        carry: and(l, r),
    };
}
/// like a normal half adder but the outputs are inversed
function notHalfAdder(l: Pin, r: Pin): {notSum: Pin, notCarry: Pin} {
    const notCarry = nor(l, r);
    return {notCarry, notSum: nor(nor(l, notCarry), nor(notCarry, r))};
}
function adder1(a: Pin, b: Pin, carry: Pin): {sum: Pin, carry: Pin} {
    const h1 = notHalfAdder(a, b);
    const h2 = notHalfAdder(h1.notSum, carry);
    return {
        sum: h2.notSum,
        carry: nor(h1.notCarry, h2.notCarry),
    };
}
function adder<W extends number>(w: W, a: Pins<W>, b: Pins<W>, carry: Pin): {sum: Pins<W>, carry: Pin} {
    const respins: Pin[] = [];
    for(let i = 0; i < w; i++) {
        const added = adder1(a[i]!, b[i]!, carry);
        carry = added.carry;
        respins.push(added.sum);
    }
    return {sum: respins as Pins<W>, carry};
}

// ok goal :: make an 8 bit cpu I guess
// instructions fit in 8 bits because why not

// wait 8 bits = max. 255 memory that's kinda small and requires paging
// why not 64 bits? (why not : because it takes 779 gates to make a single 64 bit adder)

// ok goal :: make a 64 bit cpu I guess
// or 386 gates for a 32 bit adder
// idk

// also I really want to have permission levels that would be so neat
// permission levels:
// - 0: unmapped memory
// - 1: mapped memory, no io access
// but mapped memory seems kinda difficult to implement idk
// mostly I want mapped memory because I want to make an os that has syscalls
// and is safe to run any program in

// ok so
// 16 registers (xxxx):
// - ta, tb, tc, td, te, sa, sb, sc, sd, se, ua, ub, uc, ud, ue, uf
// 1-bit flag
// registers hold 64-bit values

// (64b) instructions:
// (8b id) (4b reg) (4b reg) (48b unused) load (register : mem address).* → (register : output) // riscv does (5 reg) (3 width) (5 reg) (12 offset)
// (8b id) (4b reg) (4b reg) (48b unused) store (register : mem address).* = (register : value)
// (8b id) (4b reg) (4b reg) (4b reg) (44b unused) add (register : o) = (register : l) + (register : r) :: sets flag 0 on overflow, 1 on ok
// (8b id) (4b reg) (52b immediate) li (register : o) = (immediate52) :: *not* sign extended
// (8b id) (4b reg) write (register) :: writes the bottom 8 bits of register to the out port. the way the out port works is [8b data] [1b active].
// (8b id) (4b reg) read (register) :: reads and acknowledges 8 bits from the input port. if the read fails, flag is set to 0, else 1. does not block.

// should this be like
//   test a = b (→ flag)
// + jmp «somewhere»
// - jmp «somewhere»
// or
//   jeq a = b «somewhere»
// idk

// ok an option:
// ram can go through i/o pins
// or ram can be written straight into the logic.

// through io pins : it would take a few cycles to handle one instruction. this also means handling partial states.
// straight in logic : it would take one cycle to handle one instruction

// ok ram is going through i/o pins
// 64 bits at a time because why not
// also load/store instrs accept 64 bits
// want 8 bits? do it yourself lol
// also is this big endian or little endian? neither, memory is an []u64

// const program_counter = ;
const toggler = builtin.state(1);
toggler.set([not(toggler.value[0])]);

const incrementer = builtin.state(64);
// this uses more gates than needed because (64b)+(u1) is way easier than (64b)+(64b)+(u1)
// some automatic optimizaitons could be done (resolving gates with constant inputs) or alternatively an adder([w1] [w2] [w3]) : [max(w1, w2, w3)] could
// be made.
const incremented = adder(64, incrementer.value as Pins<64>, new Array(64).fill(0).map(q => builtin.const(0)) as Pins<64>, builtin.const(1));
incrementer.set(incremented.sum);

// instruction handling
// 1: fetch instruction at address
// 2: parse instruction, fetch needed memory

// ram:
// reading ram: set out_addr to the addres you want to read. next cycle, ram_in will contain the read value and ram_in_set will be 1.
// writing ram: set out_addr to the address you want to read, ram_out_set to 1, and ram_out_value to the value you want to set. next cycle, ram_in will
//    contain the set value and ram_in_set will be 1.
// note that no ram exists at the address 0. when ram_out_addr is set to 0, nothing will be fetched. ram_out_addr = 0 with ram_out_set = 1 is an error.

const ram_in = builtin.in("ram_in", 64);
const ram_in_available = builtin.in("ram_in_available", 1);


const instruction_ptr = builtin.state(61, "01");
const instruction_handling_stage = builtin.state(3, "000");

type Output = {
    instruction_ptr: Pins<61>,
    instruction_handling_stage: Pins<3>,

    ram_out_addr: Pins<61>,
    ram_out_set: Pins<1>,
    ram_out_set_value: Pins<64>,
};

// ifEq(pins, builtin.constw(3, "001"))
// 1 on true, 0 on false. for use with ifTrue()
function ifEq<PinsW extends Pin[]>(pins: PinsW, expected: PinsW): Pin {
    // (010) = (010)
    // (a.0 xor b.0) nor (a.1 xor b.1) nor (a.2 xor b.2)
    return nor(...pins.map((pin, i) => {
        const expcdt = expected[i]!;
        return xor(expcdt, pin);
    }));
}

type PinsMap<PinsW extends Pin[]> = {[key in keyof PinsW]: Pin};

// if the condition is true, returns ontrue
// else returns all zeroes (for use with or)
function ifTrue<PinsW extends Pin[]>(condition: Pin, ontrue: PinsW): PinsMap<PinsW> {
    return ontrue.map(ot => {
        return and(condition, ot);
    }) as PinsMap<PinsW>;
}

type IfChain<T> = {
    when(condition: Pin, value: T): IfChain<T>,
    else(value: T): T,
};
function ifChain<T>(branch: (cond: Pin, value: T) => T, merge: (a: T, b: T) => T): IfChain<T> {
    const conditions: Pin[] = []; // else is nor(...conditions)
    let cv: undefined | {t: T};
    const res: IfChain<T> = {
        when(condition, value) {
            const cond = nor(...conditions, not(condition));
            conditions.push(condition);
            const branched = branch(cond, value);
            if(cv) {
                cv = {t: merge(cv.t, branched)};
            }else{
                cv = {t: branched};
            }
            return res;
        },
        else(value) {
            if(!cv) throw new Error("when required for else");
            return merge(cv.t, branch(nor(...conditions), value));
        },
    };
    return res;
}

// instruction decoding steps:
// 0b000 - start. fetch the instruction at the ram address «instr_addr», set decoding step to 0b001
// 0b001 - decode. choose what to do based on
// 0b111 - error, everything is wrong. don't do anything and display an error LED.
function performInstructionFetch(): Output {
    return {
        instruction_ptr: instruction_ptr.value,
        instruction_handling_stage: builtin.constw(3, "001"),

        ram_out_addr: instruction_ptr.value,
        ram_out_set: builtin.constw(1, "0"),
        ram_out_set_value: builtin.constw(64, "0"),
    };
}
function performInstructionFetchArgs(): Output {
    return {
        instruction_ptr: instruction_ptr.value,
        instruction_handling_stage: builtin.constw(3, "010"),

        ram_out_addr: builtin.constw(61, "0"),
        ram_out_set: builtin.constw(1, "0"),
        ram_out_set_value: builtin.constw(64, "0010"),
    };
}
function orMany<PinsA extends Pin[]>(a: PinsA, b: PinsA): PinsMap<PinsA> {
    return a.map((av, i) => {
        const bv = b[i]!;
        return or(av, bv);
    }) as PinsMap<PinsA>;
}
function getError(): Output {
    return {
        instruction_ptr: builtin.constw(61, "0"),
        instruction_handling_stage: instruction_handling_stage.value,
        ram_out_addr: builtin.constw(61, "0"),
        ram_out_set: builtin.constw(1, "0"),
        ram_out_set_value: builtin.constw(64, "1".repeat(64)),
    };
}
function performInstructionDecode(): Output {
    // ifChain().ifEq().elseIfEq().else()
    const fetch_res = performInstructionFetch();

    return ifChain<Output>(
        (cond, value) => ({
            instruction_ptr: ifTrue(cond, value.instruction_ptr),
            instruction_handling_stage: ifTrue(cond, value.instruction_handling_stage),
            ram_out_addr: ifTrue(cond, value.ram_out_addr),
            ram_out_set: ifTrue(cond, value.ram_out_set),
            ram_out_set_value: ifTrue(cond, value.ram_out_set_value),
        }),
        (a, b) => ({
            instruction_ptr: orMany(a.instruction_ptr, b.instruction_ptr),
            instruction_handling_stage: orMany(a.instruction_handling_stage, b.instruction_handling_stage),
            ram_out_addr: orMany(a.ram_out_addr, b.ram_out_addr),
            ram_out_set: orMany(a.ram_out_set, b.ram_out_set),
            ram_out_set_value: orMany(a.ram_out_set_value, b.ram_out_set_value),
        }),
    )
        .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "000")), performInstructionFetch())
        .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "001")), performInstructionFetchArgs())
        .else(getError())
    ;
}

{
    const res = performInstructionDecode();

    instruction_ptr.set(res.instruction_ptr);
    instruction_handling_stage.set(res.instruction_handling_stage);

    const ram_out_addr = builtin.out("ram_out_addr", res.ram_out_addr);
    const ram_out_set = builtin.out("ram_out_set", res.ram_out_set);
    const ram_out_value = builtin.out("ram_out_set_value", res.ram_out_set_value);
}

// ram:
// addr >> 3 = 64b_chunk_addr

// builtin.out("counter", incremented.sum);

function assertNever(a: never): never {
    console.log("Not never:", a);
    throw new Error("Not never");
}

console.log(
`pub const Pin = union(enum) {
    in: struct { name: []const u8 },
    out: struct { name: []const u8, dep: usize },

    nor: struct { deps: [2]usize },
    constant: struct { value: u1 },
    state: struct { dep: usize, initial: u1 },
};

pub const pins = &[_]Pin{`
);
builtin.pins.forEach((pin, i) => {
    if(pin.kind === "in") {
        console.log("    .{ .in = .{ .name = "+JSON.stringify(pin.name)+" } },");
    }else if(pin.kind === "out") {
        console.log("    .{ .out = .{ .name = "+JSON.stringify(pin.name)+", .dep = "+((pin.dep as unknown as number) - 1)+" } },");
    }else if(pin.kind === "const") {
        console.log("    .{ .constant = .{ .value = "+pin.value+" } },");
    }else if(pin.kind === "nor") {
        console.log("    .{ .nor = .{ .deps = [_]usize{ "+pin.deps.map(dep => (dep as unknown as number) - 1).join(", ")+" } } },");
    }else if(pin.kind === "state") {
        if(!pin.dep) {
            console.log("};");
            console.warn(pin.debug);
            throw new Error("Missing .set for state");
        }
        console.log("    .{ .state = .{ .dep = "+((pin.dep as unknown as number) - 1)+", .initial = "+pin.initial+" } },");
    }else assertNever(pin);
});
console.log("};");

// simulate(builtin.pins);