a 64-bit cpu written in the typescript hardware description language

goals:

- cpu
- instructions and registers and stuff
- two permission levels, one full permission, one with memory accesses routed through a page table and no i/o permission
- very basic operating system that lets a user program allocate memory and read/write from/to the screen
- very basic user program that tests some stuff

build/run:

- download a deno binary for your os from [here](https://github.com/denoland/deno/releases) and put it in the root folder (next to `README.md`)
- download a latest master zig binary from [here](https://ziglang.org/download/) (being developed on `0.8.0-dev.1342+4f11a88b9`)
- `zig build run`
