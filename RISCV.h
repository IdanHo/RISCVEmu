#pragma once
#include <string>

#define XLEN 32
#define MEM_SIZE 0x10000
#define DEBUG false
#define SINGLE_STEP false

class RISCV {
	uint8_t mem[MEM_SIZE] = {};
	uint32_t mem_start_addr = 0x0;

	uint32_t regs[32] = {};
	uint32_t pc = 0x0;
	uint32_t next_pc = 0x0;

	bool is_running = false;

	void interpret_insn(uint32_t insn);
	void pretty_print(uint32_t insn);

	int read_u8(uint32_t addr, uint8_t* out);
	int read_u16(uint32_t addr, uint16_t* out);
	int read_u32(uint32_t addr, uint32_t* out);
	int write_u8(uint32_t addr, uint8_t in);
	int write_u16(uint32_t addr, uint16_t in);
	int write_u32(uint32_t addr, uint32_t in);
public:
	RISCV(const char* program_filename, uint32_t mem_start, uint32_t entrypoint);
	void run();
};

