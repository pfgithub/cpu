# Registers

r0…rE : 64 bit readable+writable regsiters

pc : 64 bit program counter, read-only. bottom 3 bits are always 0 because instructions are always 64b-aligned. writing to this register
will void the written value.

# Instructions

Format:

`(0|1 root only)(7 instruction id)`

## noop: 0b{immediate×56}{reg×4}_0000000_0

does nothing.

## li: 0b{sign×1}{immediate×51}{reg×4}_0000001_0

loads the 51-bit immediate into the specified register, sign extended with the sign bit.

## add 0b{unused×44}{reg_c×4}{reg_b×4}{reg_a×4}_0000010_0

stores `reg_c = reg_a + reg_b` and sets the overflow flag (TODO) if there is an overflow

## load 0b{unused×48}{reg_out×4}{reg_addr×4}_0000011_0

reads a 64b-aligned value from memory. `reg_b = reg_a.*`.

if the address is not 8-bit aligned, an error occurs `INVALID_ALIGN`

if memory paging is enabled and `reg_a` is not an available piece of memory, the program will exit with the code `INVALID_ACCESS`

note that `0` is never a valid address.

## store 0b{unused×48}{reg_value×4}{reg_addr×4}__0000100_0

writes a 64 bit value to the 64b-aligned memory address `reg_addr`

if the address is misaligned, error. if paging is enabled and the address does not refer to a valid page, error.

## jmp 0b{unused×52}{reg_addr×4}__0000101_0

rather than continuing at the next instruction, continues at (reg_addr)

## halt: 0b00000000000000000000000000000000000000000000000000000000_1111111_0

invalid instruction;

- as root: stops executing instructions and displays an error message through the RAM_OUT_SET_VALUE port
- as user: returns to root with the error code INVALID_INSTRUCTION

# Notes

How to implement memory paging

- Add a new u2 bit of state "memory_paging_state"
- When fetching memory, use ...getMemoryAt(position)
- getMemoryAt will:
  - return the address into the page table for step 1 and setstate `memory_paging_state=1`
- At the start of the cycle, in the main `if()`, if `memory_paging_state = 1`,
  - send this to the memory pager to advance to step 2
- Same for step 2
  - This time, the memory pager puts the actual, final address into `ram_out_addr` and sets `memory_paging_state=0`
  - Next cycle, the instruction will continue to execute as normal wherever it left off, as if nothing happened.
- How to enable paging:
- Rather than making it root vs user, instead
  - Have a root-only register "memory_paging" (set with some root-only instrs)
  - If this is set to 0, paging is disabled
  - To call user code, make sure to set the page table first. After user code returns, unset the page table.
  - Enable/disable page table as needed while in root.
  - note that this isn't actually possible and don't do this. the address of the current instruction is
    stored in a (hidden) register, but it needs to use the page table and stuff.
    having a method `set_paging «0xSOME_ADDR»` would mean that suddenly instructions
    are executing from the wrong location, and that would be bad.
    this could be fixed by having the location of the current instruction be absolute in
    memory but that would make it very easy for a user program to have the instruction pointer
    escape from mapped space if there are enough no-op instructions
