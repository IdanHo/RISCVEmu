#include "RISCV.h"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <stdexcept>

RISCV::RISCV(const char* program_filename, uint32_t mem_start, uint32_t entrypoint) {
	std::ifstream program_file(program_filename, std::ios::in | std::ios::binary);
	if (!program_file.is_open()) {
		throw std::runtime_error("Failed to open program file!");
	}
	program_file.read((char *)mem, MEM_SIZE);
	program_file.close();

	mem_start_addr = mem_start;
	pc = entrypoint;
}

void RISCV::interpret_insn(uint32_t insn) {
	uint32_t opcode = insn & 0x7F;
	if (!(opcode & 0x1 && opcode & 0x2)) {
		// TODO: throw illegal instruction exception
		return;
	}
	uint32_t rd = (insn >> 7) & 0x1F;
	uint32_t rs1 = (insn >> 15) & 0x1F;
	uint32_t rs2 = (insn >> 20) & 0x1F;
	switch (opcode) {
	case 0x13: { // Integer Register-Immediate Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t imm = ((int32_t)insn >> 20);
		uint32_t result;
		switch (funct3) {
		case 0x0: { // ADDI
			result = (int32_t)regs[rs1] + imm;
			break;
		}
		case 0x2: { // SLTI
			result = (int32_t)regs[rs1] < (int32_t)imm;
			break;
		}
		case 0x3: { // SLTIU
			result = (uint32_t)regs[rs1] < (uint32_t)imm;
			break;
		}
		case 0x4: { // XORI
			result = regs[rs1] ^ imm;
			break;
		}
		case 0x6: { // ORI
			result = regs[rs1] | imm;
			break;
		}
		case 0x7: { // ANDI
			result = regs[rs1] & imm;
			break;
		}
		case 0x1: { // SLLI
			uint32_t shamt = imm & 0x1F;
			uint32_t shtyp = (imm >> 5) & 0x7F;
			if (shtyp != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			result = regs[rs1] << shamt;
			break;
		}
		case 0x5: { // SRLI / SRAI
			uint32_t shamt = imm & 0x1F;
			uint32_t shtyp = (imm >> 5) & 0x7F;
			if ((shtyp & 0x5F) != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (shtyp) {
				result = (int32_t)regs[rs1] >> (int32_t)shamt;
			}
			else {
				result = regs[rs1] >> shamt;
			}
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		if (rd != 0)
			regs[rd] = result;
		break;
	}
	case 0x37: { // LUI
		if (rd != 0)
			regs[rd] = (int32_t)(insn & 0xFFFFF000);
		break;
	}
	case 0x17: { // AUIPC
		if (rd != 0)
			regs[rd] = pc + (int32_t)(insn & 0xFFFFF000);
		break;
	}
	case 0x33: { // Integer Register-Register Operations + Integer Multiplication Extension
		uint32_t funct3 = (insn >> 12) & 0x7;
		uint32_t funct7 = (insn >> 25) & 0x7F;
		uint32_t result;
		if (funct7 & 0x1) {
			if ((funct7 & 0x7E) != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			switch (funct3) {
			case 0x0: { // MUL
				result = (int32_t)((int32_t)regs[rs1] * (int32_t)regs[rs2]);
				break;
			}
			case 0x1: { // MULH
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (int64_t)((int32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x3: { // MULHU
				result = (uint32_t)(((uint64_t)((uint32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x2: { // MULHSU
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x4: { // DIV
				if (regs[rs2] == 0) { // Div by 0
					result = -1;
					break;
				}
				else if ((int32_t)regs[rs1] == -((int32_t)1 << (XLEN - 1)) && regs[rs2] == -1) { // Signed Div overflow
					result = regs[rs1];
					break;
				}
				result = (int32_t)((int32_t)regs[rs1] / (int32_t)regs[rs2]);
				break;
			}
			case 0x5: { // DIVU
				if (regs[rs2] == 0) {
					result = ((uint64_t)1 << XLEN) - 1;
					break;
				}
				result = regs[rs1] / regs[rs2];
				break;
			}
			case 0x6: { // REM
				if (regs[rs2] == 0) { // Div by 0
					result = regs[rs1];
					break;
				}
				else if ((int32_t)regs[rs1] == -((int32_t)1 << (XLEN - 1)) && regs[rs2] == -1) { // Signed Div overflow
					result = 0;
					break;
				}
				result = (int32_t)((int32_t)regs[rs1] % (int32_t)regs[rs2]);
				break;
			}
			case 0x7: { // REMU
				if (regs[rs2] == 0) {
					result = regs[rs1];
					break;
				}
				result = regs[rs1] / regs[rs2];
				break;
			}
			default: {
				// TODO: throw illegal instruction exception
				return;
			}
			}
		}
		else {
			if ((funct7 & 0x5F) != 0x0) {
				// TODO: throw illegal instruction exception
				return;
			}
			switch (funct3) {
			case 0x0: { // ADD / SUB
				if (funct7) {
					result = (int32_t)(regs[rs1] - regs[rs2]);
				}
				else {
					result = (int32_t)(regs[rs1] + regs[rs2]);
				}
				break;
			}
			case 0x2: { // SLT
				result = (int32_t)regs[rs1] < (int32_t)regs[rs2];
				break;
			}
			case 0x3: { // SLTU
				result = (uint32_t)regs[rs1] < (uint32_t)regs[rs2];
				break;
			}
			case 0x4: { // XOR
				result = regs[rs1] ^ regs[rs2];
				break;
			}
			case 0x6: { // OR
				result = regs[rs1] | regs[rs2];
				break;
			}
			case 0x7: { // AND
				result = regs[rs1] & regs[rs2];
				break;
			}
			case 0x1: { // SLLI
				result = regs[rs1] << (regs[rs2] & 0x1F);
				break;
			}
			case 0x5: { // SRLI / SRAI
				if (funct7) {
					result = (int32_t)regs[rs1] >> (int32_t)(regs[rs2] & 0x1F);
				}
				else {
					result = regs[rs1] >> (regs[rs2] & 0x1F);
				}
				break;
			}
			default: {
				// TODO: throw illegal instruction exception
				return;
			}
			}
		}
		if (rd != 0)
			regs[rd] = result;
		break;
	}
	case 0x6F: { // JAL
		int32_t offset = (((insn >> 21) & 0x3FF) | (((insn >> 20) & 0x1) << 10) | (((insn >> 12) & 0xFF) << 11) | ((((int32_t)insn) >> 31)) << 19) << 1;
		if (rd != 0)
			regs[rd] = next_pc;
		uint32_t addr = (int32_t)pc + offset;
		if (addr & 3) {
			// TODO: throw misaligned instruction exception
			return;
		}
		next_pc = addr;
		break;
	}
	case 0x67: { // JALR
		uint32_t funct3 = (insn >> 12) & 0x7;
		if (funct3 != 0x0) {
			// TODO: throw illegal instruction exception
			return;
		}
		int32_t offset = ((int32_t)insn) >> 20;
		if (rd != 0)
			regs[rd] = next_pc;
		uint32_t addr = (((int32_t)regs[rs1] + offset) >> 1) << 1;
		if (addr & 3) {
			// TODO: throw misaligned instruction exception
			return;
		}
		next_pc = addr;
		break;
	}
	case 0x63: { // Conditional Branches
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = (((insn >> 8) & 0xF) | (((insn >> 25) & 0x3F) << 4) | (((insn >> 7) & 0x1) << 10) | ((((int32_t)insn) >> 31)) << 11) << 1;
		bool should_branch = false;
		switch (funct3) {
		case 0x0: { // BEQ
			should_branch = regs[rs1] == regs[rs2];
			break;
		}
		case 0x1: { // BNE
			should_branch = regs[rs1] != regs[rs2];
			break;
		}
		case 0x4: { // BLT
			should_branch = (int32_t)regs[rs1] < (int32_t)regs[rs2];
			break;
		}
		case 0x6: { // BLTU
			should_branch = regs[rs1] < regs[rs2];
			break;
		}
		case 0x5: { // BGE
			should_branch = (int32_t)regs[rs1] >= (int32_t)regs[rs2];
			break;
		}
		case 0x7: { // BGEU
			should_branch = regs[rs1] >= regs[rs2];
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		if (should_branch) {
			uint32_t addr = (int32_t)pc + offset;
			if (addr & 3) {
				// TODO: throw misaligned instruction exception
				return;
			}
			next_pc = pc + offset;
		}
		break;
	}
	case 0x3: { // Load Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = ((int32_t)insn) >> 20;
		switch (funct3) {
		case 0x0: { // LB
			uint8_t result;
			if (read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int8_t)result);
			break;
		}
		case 0x4: { // LBU
			uint8_t result;
			if (read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x1: { // LH
			uint16_t result;
			if (read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int16_t)result);
			break;
		}
		case 0x5: { // LHU
			uint16_t result;
			if (read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x2: { // LW
			uint32_t result;
			if (read_u32(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = result;
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		break;
	}
	case 0x23: { // Store Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = ((insn >> 7) & 0x1F) | ((((int32_t)insn) >> 25) << 5);
		switch (funct3) {
		case 0x0: { // SB
			if (write_u8(regs[rs1] + offset, regs[rs2] & 0xFF)) {
				// TODO: handle raised exceptions
			}
			break;
		}
		case 0x1: { // SH
			if (write_u16(regs[rs1] + offset, regs[rs2] & 0xFFFF)) {
				// TODO: handle raised exceptions
			}
			break;
		}
		case 0x2: { // SW
			if (write_u32(regs[rs1] + offset, regs[rs2])) {
				// TODO: handle raised exceptions
			}
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		break;
	}
	case 0xF: { // Memory Ordering Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		switch (funct3) {
		case 0x0: { // FENCE
			// TODO: either implement fence or throw an illegal instruction exception
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		break;
	}
	case 0x73: { // System Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		uint32_t funct12 = (insn >> 20) & 0xFFF;
		switch (funct3) {
		case 0x0: { // PRIV
			if (rd != 0 || rs1 != 0) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (funct12 & 0xFFE) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (funct12) { // EBREAK
				// TODO: throw breakpoint exception
				return;
			}
			else { // ECALL
				// TODO: implement various services
				if (regs[3] & 1) { // system exit
					std::cout << "program exited with code " << (regs[3] >> 1) << std::endl;
					is_running = false;
				}
				return;
			}
			break;
		}
		default: {
			// TODO: throw illegal instruction exception
			return;
		}
		}
		break;
	}
	default: {
		// TODO: throw illegal instruction exception
		return;
	}
	}
}

int RISCV::read_u8(uint32_t addr, uint8_t* out) {
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 1) {
		*out = 0;
		std::cerr << "Illegal mem read 8, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr];
	}
	return 0;
}

int RISCV::read_u16(uint32_t addr, uint16_t* out) {
	if (addr & 1) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 2) {
		*out = 0;
		std::cerr << "Illegal mem read 16, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr] | (mem[addr + 1] << 8);
	}
	return 0;
}

int RISCV::read_u32(uint32_t addr, uint32_t* out) {
	if (addr & 3) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 4) {
		*out = 0;
		std::cerr << "Illegal mem read 32, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		*out = mem[addr] | (mem[addr + 1] << 8) | (mem[addr + 2] << 16) | (mem[addr + 3] << 24);
	}
	return 0;
}

int RISCV::write_u8(uint32_t addr, uint8_t in) {
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 1) {
		std::cerr << "Illegal mem write 8, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in;
	}
	return 0;
}

int RISCV::write_u16(uint32_t addr, uint16_t in) {
	if (addr & 1) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 2) {
		std::cerr << "Illegal mem write 16, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in & 0xFF;
		mem[addr + 1] = (in >> 8) & 0xFF;
	}
	return 0;
}

int RISCV::write_u32(uint32_t addr, uint32_t in) {
	if (addr & 3) {
		// TODO: handle misaligned read exception
		return 1;
	}
	addr -= mem_start_addr;
	if (addr < 0 || addr >= MEM_SIZE - 4) {
		std::cerr << "Illegal mem write 32, pc: " << pc << ", addr: " << addr << std::endl;
		return 1;
	}
	else {
		mem[addr] = in & 0xFF;
		mem[addr + 1] = (in >> 8) & 0xFF;
		mem[addr + 2] = (in >> 16) & 0xFF;
		mem[addr + 3] = (in >> 24) & 0xFF;
	}
	return 0;
}

void RISCV::run() {
	is_running = true;
	regs[0] = 0; // should be a noop
	regs[2] = mem_start_addr + MEM_SIZE - 4; // set stack pointer incase program assumes it exists
	while (is_running) {
		next_pc = pc + 4;

		uint32_t insn;
		if (read_u32(pc, &insn))
			throw std::runtime_error("Failed fetching next instruction!");

		if (DEBUG)
			pretty_print(insn);

		if (SINGLE_STEP)
			std::cin.ignore();

		if (insn == 0) { // TODO: remove this
			is_running = false;
		}

		interpret_insn(insn);

		pc = next_pc;
	}
}

void RISCV::pretty_print(uint32_t insn) {
	for (int i = 0; i < 32; i++) { // print regs
		std::cout << "| x" << i << ": " << regs[i] << "(" << (int32_t)regs[i] << ") ";
	}
	std::cout << "| pc: 0x" << std::setbase(16) << pc << std::setbase(10) << std::endl;

	uint32_t opcode = insn & 0x7F;
	if (!(opcode & 0x1 && opcode & 0x2)) {
		return;
	}
	uint32_t rd = (insn >> 7) & 0x1F;
	uint32_t rs1 = (insn >> 15) & 0x1F;
	uint32_t rs2 = (insn >> 20) & 0x1F;
	switch (opcode) {
	case 0x13: { // Integer Register-Immediate Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t imm = ((int32_t)insn >> 20);
		switch (funct3) {
		case 0x0: { // ADDI
			std::cout << "ADDI x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x2: { // SLTI
			std::cout << "SLTI x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x3: { // SLTIU
			std::cout << "SLTIU x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x4: { // XORI
			std::cout << "XORI x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x6: { // ORI
			std::cout << "ORI x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x7: { // ANDI
			std::cout << "ANDI x" << rd << ", x" << rs1 << ", " << imm << std::endl;
			break;
		}
		case 0x1: { // SLLI
			uint32_t shamt = imm & 0x1F;
			uint32_t shtyp = (imm >> 5) & 0x7F;
			if (shtyp != 0x0) {
				return;
			}
			std::cout << "SLLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl;
			break;
		}
		case 0x5: { // SRLI / SRAI
			uint32_t shamt = imm & 0x1F;
			uint32_t shtyp = (imm >> 5) & 0x7F;
			if ((shtyp & 0x5F) != 0x0) {
				return;
			}
			if (shtyp) {
				std::cout << "SRAI x" << rd << ", x" << rs1 << ", " << shamt << std::endl;
			}
			else {
				std::cout << "SRLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl;
			}
			break;
		}
		}
		break;
	}
	case 0x37: { // LUI
		std::cout << "LUI x" << rd << ", " << (int32_t)(insn & 0xFFFFF000) << std::endl;
		break;
	}
	case 0x17: { // AUIPC
		std::cout << "AUIPC x" << rd << ", " << (int32_t)(insn & 0xFFFFF000) << std::endl;
		break;
	}
	case 0x33: { // Integer Register-Register Operations + Integer Multiplication Extension
		uint32_t funct3 = (insn >> 12) & 0x7;
		uint32_t funct7 = (insn >> 25) & 0x7F;
		if (funct7 & 0x1) {
			if ((funct7 & 0x7E) != 0x0) {
				return;
			}
			switch (funct3) {
			case 0x0: { // MUL
				std::cout << "MUL x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x1: { // MULH
				std::cout << "MULH x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x3: { // MULHU
				std::cout << "MULHU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x2: { // MULHSU
				std::cout << "MULHSU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x4: { // DIV
				std::cout << "DIV x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x5: { // DIVU
				std::cout << "DIVU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x6: { // REM
				std::cout << "REM x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x7: { // REMU
				std::cout << "REMU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			}
		}
		else {
			if ((funct7 & 0x5F) != 0x0) {
				return;
			}
			switch (funct3) {
			case 0x0: { // ADD / SUB
				if (funct7) {
					std::cout << "SUB x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				}
				else {
					std::cout << "ADD x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				}
				break;
			}
			case 0x2: { // SLT
				std::cout << "SLT x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x3: { // SLTU
				std::cout << "SLTU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x4: { // XOR
				std::cout << "XOR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x6: { // OR
				std::cout << "OR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x7: { // AND
				std::cout << "AND x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x1: { // SLLI
				std::cout << "SLLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				break;
			}
			case 0x5: { // SRLI / SRAI
				if (funct7) {
					std::cout << "SRAI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				}
				else {
					std::cout << "SRLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl;
				}
				break;
			}
			}
		}
		break;
	}
	case 0x6F: { // JAL
		int32_t offset = (((insn >> 21) & 0x3FF) | (((insn >> 20) & 0x1) << 10) | (((insn >> 12) & 0xFF) << 11) | ((((int32_t)insn) >> 31)) << 19) << 1;
		std::cout << "JAL x" << rd << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
		break;
	}
	case 0x67: { // JALR
		uint32_t funct3 = (insn >> 12) & 0x7;
		if (funct3 != 0x0) {
			return;
		}
		int32_t offset = ((int32_t)insn) >> 20;
		std::cout << "JALR x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
		break;
	}
	case 0x63: { // Conditional Branches
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = (((insn >> 8) & 0xF) | (((insn >> 25) & 0x3F) << 4) | (((insn >> 7) & 0x1) << 10) | ((((int32_t)insn) >> 31)) << 11) << 1;
		switch (funct3) {
		case 0x0: { // BEQ
			std::cout << "BEQ x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x1: { // BNE
			std::cout << "BNE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x4: { // BLT
			std::cout << "BLT x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x6: { // BLTU
			std::cout << "BLTU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x5: { // BGE
			std::cout << "BGE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x7: { // BGEU
			std::cout << "BGEU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		}
		break;
	}
	case 0x3: { // Load Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = ((int32_t)insn) >> 20;
		switch (funct3) {
		case 0x0: { // LB
			std::cout << "LB x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x4: { // LBU
			std::cout << "LBU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x1: { // LH
			std::cout << "LH x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x5: { // LHU
			std::cout << "LHU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		case 0x2: { // LW
			std::cout << "LW x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl;
			break;
		}
		}
		break;
	}
	case 0x23: { // Store Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		int32_t offset = ((insn >> 7) & 0x1F) | ((((int32_t)insn) >> 25) << 5);
		switch (funct3) {
		case 0x0: { // SB
			std::cout << "SB x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl;
			break;
		}
		case 0x1: { // SH
			std::cout << "SH x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl;
			break;
		}
		case 0x2: { // SW
			std::cout << "SW x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl;
			break;
		}
		}
		break;
	}
	case 0xF: { // Memory Ordering Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		switch (funct3) {
		case 0x0: { // FENCE
			std::cout << "FENCE ???" << std::endl;
			break;
		}
		}
		break;
	}
	case 0x73: { // System Instructions
		uint32_t funct3 = (insn >> 12) & 0x7;
		uint32_t funct12 = (insn >> 20) & 0xFFF;
		switch (funct3) {
		case 0x0: { // PRIV
			if (rd != 0 || rs1 != 0) {
				return;
			}
			if (funct12 & 0xFFE) {
				return;
			}
			if (funct12) { // EBREAK
				std::cout << "EBREAK" << std::endl;
				return;
			}
			else { // ECALL
				std::cout << "ECALL" << std::endl;
				return;
			}
			break;
		}
		}
		break;
	}
	}
}