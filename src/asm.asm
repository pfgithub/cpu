(jump :os)

.r0 = (li 0x0)
{
    .t1 = (li 0x79A)
    .t2 = (li 0x347A)
    .t1 = (add .t1 .t2)
}
{
    .t1 = (li 0o10)
    .t2 = (load .t1)
}
{
    .t1 = (li 0x0)
    .t2 = (li 0o20)
    .t2 = (add .t2 .pc)
    (store .t2 .t1)
    halt_lbl: (halt)
}
{
    (jump :skip)
    (halt)
    skip:
}
{
    .t1 = (li 0x12)
    .t2 = (li -0x83)
    .t1 = (add .t1 .t2)
}
{
    .t0 = (li -0x7)
    .t1 = (li -0x12)
    
    (call .ra :add_fn .s0 .s1 .s2 .s3 .s4 .s5)
}
.r0 = (li 0xAAAAAA)
(halt)

add_fn: {
    // (TODO clr_regs …all regs)
    .t0 = (add .t0 .t1)
    (ret .ra)
}

// ok.
// register allocator needs two types of jumps
// (call .ta :addr .a0 .a1 .a2) // a jump to a pc-relative offset, returns. args: ret_addr address, immediate offset, …saved_registers[]
// (jmp :addr) // a jump to a pc-relative offset, `noreturn`
// (condjmp :addr) // a conditional jump, indicates that code will either resume at addr or next instruction. (no conditional jumps are implemented yet)
// (ret .ta) // a jump to an address, `noreturn`.
// sample:

//(halt)
/// function demo_fn(.a0, .a1) .a0
//demo_fn: {
//    .a0 = (add .a0 .a1)
//    (ret .ta) // for the register allocator, does not have a return address
//}


os: {

    .t0 = (li [8x 0b1][8x 0b0][8x 0b0][8x 'h'])
    (write .t0)

}


//
//string: [8x 'h'][8x 'e'][8x 'l'][8x 'l'][8x 'o'][8x 0][8x 0][8x 0]












































//
