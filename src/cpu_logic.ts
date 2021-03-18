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
    : N extends 56
    ? [
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
    ]
    : N extends 52
    ? [
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
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
        if(initial.length > w) throw new Error("bad constw");
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

const current_instruction = builtin.state(64);

type RegisterSet = Tuple<Pins<64>, 16>;
const registers_array = new Array(16).fill(0).map(q => builtin.state(64));

const registers: RegisterSet = registers_array.map(reg => reg.value) as RegisterSet;

// const page_table_ptr = builtin.state(61, "0"); // the value of this determines if the cpu is in user or root mode
// run_user «reg:page_table_addr» «reg:start_addr(user)»
// :: this does a few things
//  - stores exception_jmp = (instruction_ptr + 1) 
//  - sets page table
//  - jumps to the specified instruction

// returns an updated set of registers with register register_id set to register_value
function setRegister(registers: RegisterSet, register_id: Pins<4>, register_value: Pins<64>): RegisterSet {
    return registers.map((value, i): Pins<64> => {
        const condition = ifEq(register_id, builtin.constw(4, i.toString(2)));
        return orMany(
            ifTrue(condition, register_value),
            ifTrue(not(condition), value),
        );
    }) as RegisterSet;
}
// returns the value of the register with the given name
function getRegister(registers: RegisterSet, register_id: Pins<4>): Pins<64> {
    let res = builtin.constw(64, "0");
    registers.forEach((register_value, i) => {
        res = orMany(res, ifTrue(ifEq(register_id, builtin.constw(4, i.toString(2))), register_value));
    });
    return res;
}
// set two registers: eg
// setRegister(setRegister(registers, r1_id, r1_value), r2_id, r2_value)

type RamPins = {
    out_addr: Pins<61>,
    out_set: Pins<1>,
    out_set_value: Pins<64>,
};
type Output = {
    instruction_ptr: Pins<61>,
    instruction_handling_stage: Pins<3>,

    ram: RamPins,
    registers: RegisterSet,

    current_instruction: Pins<64>,
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
const outputNCIBranch = (cond: Pin, value: NCIOutput): NCIOutput => ({
    instruction_ptr: ifTrue(cond, value.instruction_ptr),
    instruction_handling_stage: ifTrue(cond, value.instruction_handling_stage),
    ram: {
        out_addr: ifTrue(cond, value.ram.out_addr),
        out_set: ifTrue(cond, value.ram.out_set),
        out_set_value: ifTrue(cond, value.ram.out_set_value),
    },
    registers: value.registers.map((reg, i) => {
        return ifTrue(cond, reg);
    }) as RegisterSet,
});
const outputBranch = (cond: Pin, value: Output): Output => ({
    ...outputNCIBranch(cond, value),
    current_instruction: ifTrue(cond, value.current_instruction),
})
const outputNCIMerge = (a: NCIOutput, b: NCIOutput): NCIOutput => ({
    instruction_ptr: orMany(a.instruction_ptr, b.instruction_ptr),
    instruction_handling_stage: orMany(a.instruction_handling_stage, b.instruction_handling_stage),
    ram: {
        out_addr: orMany(a.ram.out_addr, b.ram.out_addr),
        out_set: orMany(a.ram.out_set, b.ram.out_set),
        out_set_value: orMany(a.ram.out_set_value, b.ram.out_set_value),
    },
    registers: a.registers.map((reg, i) => {
        return orMany(reg, b.registers[i]!);
    }) as RegisterSet,
});
const outputMerge = (a: Output, b: Output): Output => ({
    ...outputNCIMerge(a, b),
    current_instruction: orMany(a.current_instruction, b.current_instruction),
});

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
        ...increment_instruction_ptr,
        
        registers: registers,

        current_instruction: builtin.constw(64, "0"),
    };
}

// these functions will initiate the 3-step page table reading process if necessary
function fetchRam(addr: Pins<61>): RamPins {
    return {
        out_addr: addr,
        out_set: builtin.constw(1, "0"),
        out_set_value: builtin.constw(64, "0"),
    };
}
function setRam(addr: Pins<61>, value: Pins<64>): RamPins {
    return {
        out_addr: addr,
        out_set: builtin.constw(1, "1"),
        out_set_value: value,
    };
}
const no_ram: RamPins = {
    out_addr: builtin.constw(61, "0"),
    out_set: builtin.constw(1, "0"),
    out_set_value: builtin.constw(64, "0"),
};

const increment_instruction_ptr = ((): {instruction_ptr: Pins<61>, instruction_handling_stage: Pins<3>, ram: RamPins} => {
    const incremented = adder(61, instruction_ptr.value, builtin.constw(61, "0"), builtin.const(1)).sum;
    return {
        instruction_ptr: incremented,
        instruction_handling_stage: builtin.constw(3, "001"),

        ram: fetchRam(instruction_ptr.value),
    };
})();
type NCIOutput = Omit<Output, "current_instruction">;
const instructions: {[key: string]: {eval: (args: Pins<56>) => NCIOutput}} = {
    /// NOOP (unused×56)
    "00000000": {eval: (args: Pins<56>) => {
        return {
            ...increment_instruction_ptr,

            registers: registers,
        };
    }},
    /// LI (reg×4)(immediate×52)
    "00000010": {eval: (args: Pins<56>) => {
        const register_id = args.slice(0, 4) as Pins<4>;
        const immediate = args.slice(4, 56) as Pins<52>;
        return {
            ...increment_instruction_ptr,

            registers: setRegister(registers, register_id, [...immediate, ...builtin.constw(12, "0")]),
        };
    }},
    /// ADD (reg×4)(reg×4)(reg×4)(unused×44)
    "00000100": {eval: (args: Pins<56>) => {
        const reg_a_in = args.slice(0, 4) as Pins<4>;
        const reg_b_in = args.slice(4, 8) as Pins<4>;
        const reg_out = args.slice(8, 12) as Pins<4>;

        const add_res = adder(64, getRegister(registers, reg_a_in), getRegister(registers, reg_b_in), builtin.const(0));
        return {
            ...increment_instruction_ptr,

            registers: setRegister(registers, reg_out, add_res.sum),
            // TODO set overflow flag on overflow
        };
    }},
    // LOAD (load_addr×4)(out_reg×4)(unused×48)
    "00000110": {eval: (args: Pins<56>): NCIOutput => {
        // 2 steps. 1: read memory. 2: set register.
        const load_addr_reg = getRegister(registers, args.slice(0, 4) as Pins<4>);
        const out_reg = args.slice(4, 8) as Pins<4>;

        const load_addr = load_addr_reg.slice(3, 64) as Pins<61>; // first 3 bits are ignored
        const expct_zero = load_addr_reg.slice(0, 3);

        return ifChain(outputNCIBranch, outputNCIMerge)
            // address must be 8 bit aligned
            .when(not(ifEq(expct_zero, builtin.constw(3, "000"))), getError(builtin.constw(64, 0xB4D41167.toString(2))))
            .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "001")), ((): NCIOutput => ({
                instruction_ptr: instruction_ptr.value,
                instruction_handling_stage: builtin.constw(3, "010"),

                ram: fetchRam(load_addr),

                registers: registers,
            }))())
            .else(((): NCIOutput => ({
                ...increment_instruction_ptr,

                registers: setRegister(registers, out_reg, ram_in),
            }))())
        ;
    }},
    // STORE (store_addr×4)(value_reg×4)(unused×48)
    "00001000": {eval: (args: Pins<56>): NCIOutput => {
        // 2 steps. 1: read memory. 2: set register.
        const store_addr_reg = getRegister(registers, args.slice(0, 4) as Pins<4>);
        const store_value = getRegister(registers, args.slice(4, 8) as Pins<4>);

        const store_addr = store_addr_reg.slice(3, 64) as Pins<61>; // first 3 bits are ignored
        const expct_zero = store_addr_reg.slice(0, 3);

        return ifChain(outputNCIBranch, outputNCIMerge)
            // address must be 8 bit aligned
            .when(not(ifEq(expct_zero, builtin.constw(3, "000"))), getError(builtin.constw(64, 0xB4D41167.toString(2))))
            .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "001")), ((): NCIOutput => ({
                instruction_ptr: instruction_ptr.value,
                instruction_handling_stage: builtin.constw(3, "010"),

                ram: setRam(store_addr, store_value),

                registers: registers,
            }))())
            .else(((): NCIOutput => ({
                ...increment_instruction_ptr,

                registers: registers,
            }))())
        ;
    }},
} as const;
const perform_instruction_fetch_args = ((): Output => {
    // the instruction is in `ram_in`
    // :: get the instruction id

    // oh the problem is if instruction_handling_stage == 0b001, current_instruction = ram_in
    const current_instr = ifChain<Pins<64>>(
        (cond, a) => ifTrue(cond, a),
        (a, b) => orMany(a, b),
    ).when(ifEq(instruction_handling_stage.value, builtin.constw(3, "001")), ram_in).else(current_instruction.value);

    const instr_id = current_instr.slice(0, 8) as Pins<8>;
    const instr_args = current_instr.slice(8, 64) as Pins<56>;

    const chain = ifChain(outputBranch, outputMerge);
    for(const [key, value] of Object.entries(instructions)) {
        chain.when(ifEq(instr_id, builtin.constw(8, key)), {...value.eval(instr_args), current_instruction: current_instr});
    }
    return chain.else(getError([...builtin.constw(56, 0xBADC0DE.toString(2)), ...instr_id]));
})();
function orMany<PinsA extends Pin[]>(a: PinsA, b: PinsA): PinsMap<PinsA> {
    return a.map((av, i) => {
        const bv = b[i]!;
        return or(av, bv);
    }) as PinsMap<PinsA>;
}
function getError(err_code: Pins<64>): Output {
    return {
        instruction_ptr: builtin.constw(61, "0"),
        instruction_handling_stage: builtin.constw(3, "111"),
        ram: {
            out_addr: builtin.constw(61, "0"),
            out_set: builtin.constw(1, "0"),
            out_set_value: err_code,
        },
        registers: registers,
        current_instruction: builtin.constw(64, "0"),
    };
}
function performInstructionDecode(): Output {
    // ifChain().ifEq().elseIfEq().else()
    const fetch_res = performInstructionFetch();

    return ifChain<Output>(outputBranch, outputMerge)
        .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "000")), performInstructionFetch())
        .when(ifEq(instruction_handling_stage.value, builtin.constw(3, "111")), getError(ifTrue(toggler.value[0], builtin.constw(64, "1".repeat(64)))))
        .else(perform_instruction_fetch_args)
    ;
}

{
    const res = performInstructionDecode();

    instruction_ptr.set(res.instruction_ptr);
    instruction_handling_stage.set(res.instruction_handling_stage);
    
    const ram_out_addr = builtin.out("ram_out_addr", res.ram.out_addr);
    const ram_out_set = builtin.out("ram_out_set", res.ram.out_set);
    const ram_out_value = builtin.out("ram_out_set_value", res.ram.out_set_value);

    res.registers.forEach((reg, i) => {
        registers_array[i]!.set(reg);
    });
    res.registers.forEach((reg, i) => {
        builtin.out("r"+i.toString(16).toUpperCase(), reg);
    });

    current_instruction.set(res.current_instruction);
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