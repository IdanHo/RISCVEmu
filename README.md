# RISC-V Emulator
This is a [RISC-V](https://en.wikipedia.org/wiki/RISC-V) emulator written in C++ for Windows & Linux, using no external libraries (besides the standard c++ library).
Currently only the RV32I base ISA (besides FENCE), and the M extension are implemented

## How to use
Binaries for the emulator can be created using:
```bash
riscv32-unknown-elf-gcc -Wl,-Ttext=0x0 -nostdlib -march=rv32im -o out_binary source.c
riscv32-unknown-elf-objcopy -O binary out_binary out_binary.bin
```
and then ran using:
```bash
RISCVEmu out_binary.bin entrypoint_address
```
the address of the entrypoint (usually the main function in c/c++) can be found using:
```bash
riscv32-unknown-elf-objdump -t out_binary
```

## TODO
- Implement Zicsr Extension (to support defining the exception vector)
- Implement Exception Delegation
- Implement F (and possibly D) ISA Extensions to fully support RV32G
- Parse RISC-V ELF & PE files to automatically determine the base address and the entrypoint address
- Optional: Upgrade to RV64I

## References
The code is based on the [RISC-V Unprivileged ISA Specification](https://riscv.org/technical/specifications/).