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
    // | {kind: "store", dep: Pin, initial: 0 | 1}
    | {kind: "not", dep: Pin}
    | {kind: "nor", deps: [Pin, Pin]}
    | {kind: "const", value: 0 | 1}
;
const builtin = {
    pins: [] as PinData[],
    pin_cache: new Map<string, Pin>(),
    addPin(data: PinData): Pin {
        const stringified = JSON.stringify(data);
        const cached = builtin.pin_cache.get(stringified);
        if(data.kind !== "in" && data.kind !== "out" && cached) return cached; // it's fine for stores to be deduplicated
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
        return builtin.addPin({kind: "nor", deps: [a, b]});
    },
    not(a: Pin): Pin {
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
    return rest.reduce((t, c) => xor(t, c), or(
        and(a, not(b)),
        and(b, not(a)),
    ));
}
function adder1(a: Pin, b: Pin, carry: Pin): {sum: Pin, carry: Pin} {
    const mid = nor(a, b);
    const sect = nor(nor(a, mid), nor(mid, b));
    const midsect = nor(sect, carry);
    return {
        sum: nor(a, sect),
        carry: nor(nor(sect, midsect), nor(midsect, carry)),
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

// instructions:
// load (register : mem address).* → (register : output)
// store (register : mem address).* = (register : value)
// add (register : o) = (register : l) + (register : r) :: sets flag 1 on overflow, 0 on ok
// mov (register : o) = (immediate32)

// should this be like
//   test a = b (→ flag)
// + jmp «somewhere»
// - jmp «somewhere»
// or
//   jeq a = b «somewhere»
// idk

function assertNever(a: never): never {
    console.log("Not never:", a);
    throw new Error("Not never");
}
function simulate(raw_pins: PinData[]) {
    type SimulationPin =
        | {kind: "const", value: boolean}
        | {kind: "pin", dep: Pin}
        | {kind: "not", dep: Pin}
        | {kind: "nor", deps: Pins<2>}
    ;
    const inputs: {[key: string]: (0 | 1)[]} = {
        left: [0, 0, 1, 0],
        right: [0, 1, 0, 0],
    };
    const outputs: {pin: Pin, name: string}[] = [];
    const sim_pins: SimulationPin[] = raw_pins.map((pin, index): SimulationPin => {
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
            outputs.push({pin: (index + 1) as unknown as Pin, name: pin.name});
            return {kind: "pin", dep: pin.dep};
        }else assertNever(pin);
    });
    const pin_cache: boolean[] = [];

    function getValueNoCache(pv: SimulationPin): boolean {
        if(pv.kind === "const") return pv.value;
        if(pv.kind === "nor") return !(getValue(pv.deps[0]) || getValue(pv.deps[1]));
        if(pv.kind === "not") return !getValue(pv.dep);
        if(pv.kind === "pin") return getValue(pv.dep);
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
    console.log(outputs_map);
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
    }else assertNever(pin);
});
simulate(builtin.pins);