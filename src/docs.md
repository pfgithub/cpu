# Instructions

Format:

`(0|1 root only)(7 instruction id)`

## li: 0b{immediate×52}{reg×4}_0000001_0

loads the 52-bit immediate into the specified register. not sign extended.

## (root only) test: 0b00000000000000000000000000000000000000000000000000000000_0001000_1

displays "0xFEEDC0DE" (4276992702) on the "ram_out_set_value" port, with "ram_out_set" = 0

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
