zero = (li 0x0)
{
    a = (li 0x79A)
    b = (li 0x347A)
    c = (add a b)
}
{
    addr = (li 0o10)
    result = (load addr)
}
{
    replace_value = (li 0x0)
    replace_addr = (add (li 0d3) (add #pc (li :halt_lbl)))
    (store replace_addr replace_value)
    halt_lbl: (halt)
}
{
    (jump :skip)
    (halt)
    skip:
}
{
    a = (li 0x12)
    b = (li -0x83)
    c = (add a  b)
}
#pc = (li 0d0)
(halt)

// ok.
// register allocator needs two types of jumps
// (call #ra :addr #a0 #a1 #a2) // a jump to a pc-relative offset, returns. args: ret_addr address, immediate offset, …saved_registers[]
// (jmp :addr) // a jump to a pc-relative offset, `noreturn`
// (condjmp :addr) // a conditional jump, indicates that code will either resume at addr or next instruction. (no conditional jumps are implemented yet)
// (ret #ra) // a jump to an address, `noreturn`.
// sample:

//(halt)
/// function demo_fn(#a0, #a1) #a0
//demo_fn: {
//    #a0 = (add #a0 #a1)
//    (ret #ra) // for the register allocator, does not have a return address
//}