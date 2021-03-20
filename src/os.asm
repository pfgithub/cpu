(add (li 0x79a) (li 0x347a) r2)
(jne r2 (li 0x3C14) (li &panic))
(load (<< 1 3))
(store (li &replace_this_instr) (li (0xC0DE0000102)))
(:replace_this_instr)(li r1 (0xBAD))
(testeq r1 (li 0xC0DE0000) (li &panic))
(jmp (li &jmp_res))
(halt)
(:jmp_res)(li .r2 0x11C0DE55)
(halt)

(:panic)(halt)

asm.add(asm.li(0x79), asm.li(0x347a), reg.r2)

// call user code:

(call &setup_page_table)

(:event_loop)


(halt)

(:setup_page_table)(blk
    (li .r0 0)
    (li .r3 0x8000)
    (blk (:loop)
        (store (add .r0 .r3) (li 0))
        (add .r0 (li (<< 1 3)) .r0)
        (eq .r0 (li 0x9000))
    (-jmp &loop))
    (li .r4 0x9000)
    (store .r3 .r4)
    (li .r0 0)
    (blk (:loop)
        (store (add .r0 .r4) (li 0))
        (add .r0 (li (<< 1 3)) .r0)
        (eq .r0 0x10_000)
    (-jmp &loop))
    (li .r4 0x10_000)
    (store )
    // ok actually 3-wide could work 0xFFF FFF FFF FFF FFFF, 4-level page table.
(ret))