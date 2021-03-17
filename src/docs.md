# Instructions

Format:

`(0|1 root only)(7 instruction id)`

## (root only) test: 0b00000000000000000000000000000000000000000000000000000000_0001000_1

displays "0xFEEDC0DE" (4276992702) on the "ram_out_set_value" port, with "ram_out_set" = 0

## halt: 0b00000000000000000000000000000000000000000000000000000000_1111111_0

invalid instruction;

- as root: stops executing instructions and displays an error message through the RAM_OUT_SET_VALUE port
- as user: returns to root with the error code INVALID_INSTRUCTION
