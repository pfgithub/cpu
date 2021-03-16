type Tuple<T, N extends number> = N extends 64
    ? [
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
    ]
    : N extends N ? number extends N ? T[] : _TupleOf<T, N, []> : never;
type _TupleOf<T, N extends number, R extends unknown[]> = R['length'] extends N ? R : _TupleOf<T, N, [T, ...R]>;

type Pins<L extends number> = Tuple<Pin, L>;
type Pin = {__opaque_is_pin: true};
type PinData =
    | {kind: "in", name: string}
    | {kind: "out", name: string, dep: Pin}
    | {kind: "state", dep: Pin | undefined, initial: 0 | 1}
    | {kind: "not", dep: Pin}
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
    not(a: Pin): Pin {
        const pv = builtin.getPin(a);
        if(pv.kind === "const") return builtin.addPin({kind: "const", value: pv.value === 1 ? 0 : 1});
        return builtin.addPin({kind: "not", dep: a});
    },
    out<Pins extends Pin[]>(name: string, pins: Pins): Pins {
        return pins.map(pin =>
            builtin.addPin({kind: "out", dep: pin, name})
        ) as unknown as Pins; // this is okay because Pin can't be extended
    },
    const(value: 0 | 1): Pin {
        return builtin.addPin({kind: "const", value});
    },
    state<Length extends number>(width: Length): Tuple<StateItem, Length> {
        const res: StateItem[] = new Array(width).fill(0).map((): StateItem => {
            const pin_data: PinData = {kind: "state", dep: undefined, initial: 0};
            return {value: builtin.addPin(pin_data), setValue: (dep) => {
                if(pin_data.dep) throw new Error("dep already set");
                pin_data.dep = dep;
            }};
        });
        return res as Tuple<StateItem, Length>;
    }
};

function nor(a: Pin, b: Pin, ...rest: Pin[]): Pin {
    return rest.reduce((t, c) => builtin.nor(t, c), builtin.nor(a, b));
}
function not(a: Pin): Pin {
    return builtin.not(a);
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
function adder1(a: Pin, b: Pin, carry: Pin): {sum: Pin, carry: Pin} {
    const h1 = halfAdder(a, b); // 1 + 1 = s0 c1
    const h2 = halfAdder(h1.sum, carry); // 0 + 0 = s0 c0
    return {
        sum: h2.sum,
        carry: not(nor(h1.carry, h2.carry)),
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
const added = adder(64, builtin.in("left", 64), builtin.in("right", 64), builtin.const(0));
builtin.out("total", [...added.sum, added.carry]);

const added1 = adder(1, builtin.in("add1l", 1), builtin.in("add1r", 1), builtin.in("add1c", 1)[0]);
builtin.out("added1", [...added1.sum, added1.carry]);

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

// const program_counter = ;
const toggler = builtin.state(1);
toggler[0].setValue(not(toggler[0].value));

const incrementer = builtin.state(64);
// this uses more gates than needed because (64b)+(u1) is way easier than (64b)+(64b)+(u1)
// some automatic optimizaitons could be done (resolving gates with constant inputs) or alternatively an adder([w1] [w2] [w3]) : [max(w1, w2, w3)] could
// be made.
const incremented = adder(64, incrementer.map(q => q.value) as Pins<64>, new Array(64).fill(0).map(q => builtin.const(0)) as Pins<64>, builtin.const(1));
incremented.sum.forEach((v, i) => incrementer[i]!.setValue(v));

builtin.out("counter", incremented.sum);

function assertNever(a: never): never {
    console.log("Not never:", a);
    throw new Error("Not never");
}
function simulate(raw_pins: PinData[]) {
    type SimulationPin =
        | {kind: "const", value: boolean}
        | {kind: "state", id: number}
        | {kind: "pin", dep: Pin}
        | {kind: "not", dep: Pin}
        | {kind: "nor", deps: Pins<2>}
    ;
    const inputs: {[key: string]: (0 | 1)[]} = {
        left: "1011001".padStart(64, "0").split("").reverse().map(v => v === "1" ? 1 : 0),
        right: "1011011".padStart(64, "0").split("").reverse().map(v => v === "1" ? 1 : 0),

        add1l: [1],
        add1r: [1],
        add1c: [0],
    };
    const outputs: {pin: Pin, name: string}[] = [];
    let state_values: {value: boolean, dep: Pin}[] = [];
    const sim_pins: SimulationPin[] = raw_pins.map((pin, index): SimulationPin => {
        const pin_id = (index + 1) as unknown as Pin;
        if(pin.kind === "const") {
            return {kind: "const", value: pin.value === 1};
        }else if(pin.kind === "nor") {
            return {kind: "nor", deps: pin.deps};
        }else if(pin.kind === "not") {
            return {kind: "not", dep: pin.dep};
        }else if(pin.kind === "in") {
            const inv = inputs[pin.name] ?? [];
            const ina = inv.shift();
            if(ina == null) throw new Error("missing inv: "+pin.name);
            return {kind: "const", value: ina === 1};
        }else if(pin.kind === "out") {
            outputs.push({pin: pin_id, name: pin.name});
            return {kind: "pin", dep: pin.dep};
        }else if(pin.kind === "state") {
            if(!pin.dep) throw new Error("State missing dep!");
            const sid = state_values.push({value: pin.initial === 1, dep: pin.dep}) - 1;
            return {kind: "state", id: sid};
        }else assertNever(pin);
    });
    function executeOneCycle(): {[key: string]: number[]} {
        const pin_cache: boolean[] = [];

        function getValueNoCache(pv: SimulationPin): boolean {
            if(pv.kind === "const") return pv.value;
            if(pv.kind === "nor") return !(getValue(pv.deps[0]) || getValue(pv.deps[1]));
            if(pv.kind === "not") return !getValue(pv.dep);
            if(pv.kind === "pin") return getValue(pv.dep);
            if(pv.kind === "state") return state_values[pv.id]!.value;
            assertNever(pv);
        }
        function getValue(pin: Pin): boolean {
            const cached = pin_cache[pin as unknown as number];
            if(cached != null) return cached;
            const pv = sim_pins[(pin as unknown as number) - 1]!;
            const res = getValueNoCache(pv);
            pin_cache[pin as unknown as number] = res;
            return res;
        }
        const outputs_map: {[key: string]: number[]} = {};
        for(const output of outputs) {
            if(!Object.hasOwnProperty.call(outputs_map, output.name)) outputs_map[output.name] = [];
            outputs_map[output.name]!.push(+getValue(output.pin));
        }
        state_values = state_values.map(sv => {
            return {value: getValue(sv.dep), dep: sv.dep};
        });

        return outputs_map;
    }
    console.log(executeOneCycle());
    console.log(executeOneCycle());
    console.log(executeOneCycle());
    console.log(executeOneCycle());
    console.log(executeOneCycle());
    console.log(executeOneCycle());
}
builtin.pins.forEach((pin, i) => {
    if(pin.kind === "in") {
        console.log("%"+(i + 1)+" = in "+pin.name);
    }else if(pin.kind === "out") {
        console.log("%"+(i + 1)+" = out %"+pin.dep+" : "+pin.name);
    }else if(pin.kind === "const") {
        console.log("%"+(i + 1)+" = pin.value");
    }else if(pin.kind === "nor") {
        console.log("%"+(i + 1)+" = nor "+pin.deps.map(dep => "%"+dep).join(" "));
    }else if(pin.kind === "not") {
        console.log("%"+(i + 1)+" = not %"+pin.dep);
    }else if(pin.kind === "state") {
        console.log("%"+(i + 1)+" = @state(%"+(pin.dep ?? "ERR")+", "+pin.initial+")");
    }else assertNever(pin);
});
simulate(builtin.pins);