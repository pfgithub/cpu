a 64-bit cpu written in the typescript hardware description language

build/run:

- install node and the yarn package manager.
- `yarn install`
- download a latest master zig binary from [here](https://ziglang.org/download/) (being developed on `0.8.0-dev.1342+4f11a88b9`)
- `zig build run` (note `node` must be in your path)

status:

- supports a few instructions (check [docs](src/docs.md) for a list of supported instructions)

goals:

- cpu
- instructions and registers and stuff
- two permission levels, one full permission, one with memory accesses routed through a page table and no i/o permission
- very basic operating system that lets a user program allocate memory and read/write from/to the screen
- very basic user program that tests some stuff