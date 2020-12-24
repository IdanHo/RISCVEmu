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
	program_file.read((char*)mem, MEM_SIZE);
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
			DEBUG(std::cout << "ADDI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (int32_t)regs[rs1] + imm;
			break;
		}
		case 0x2: { // SLTI
			DEBUG(std::cout << "SLTI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (int32_t)regs[rs1] < (int32_t)imm;
			break;
		}
		case 0x3: { // SLTIU
			DEBUG(std::cout << "SLTIU x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = (uint32_t)regs[rs1] < (uint32_t)imm;
			break;
		}
		case 0x4: { // XORI
			DEBUG(std::cout << "XORI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = regs[rs1] ^ imm;
			break;
		}
		case 0x6: { // ORI
			DEBUG(std::cout << "ORI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
			result = regs[rs1] | imm;
			break;
		}
		case 0x7: { // ANDI
			DEBUG(std::cout << "ANDI x" << rd << ", x" << rs1 << ", " << imm << std::endl);
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
			DEBUG(std::cout << "SLLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
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
				DEBUG(std::cout << "SRAI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
				result = (int32_t)regs[rs1] >> (int32_t)shamt;
			}
			else {
				DEBUG(std::cout << "SRLI x" << rd << ", x" << rs1 << ", " << shamt << std::endl);
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
		DEBUG(std::cout << "LUI x" << rd << ", " << (int32_t)(insn & 0xFFFFF000) << std::endl);
		if (rd != 0)
			regs[rd] = (int32_t)(insn & 0xFFFFF000);
		break;
	}
	case 0x17: { // AUIPC
		DEBUG(std::cout << "AUIPC x" << rd << ", " << (int32_t)(insn & 0xFFFFF000) << std::endl);
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
				DEBUG(std::cout << "MUL x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)((int32_t)regs[rs1] * (int32_t)regs[rs2]);
				break;
			}
			case 0x1: { // MULH
				DEBUG(std::cout << "MULH x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (int64_t)((int32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x3: { // MULHU
				DEBUG(std::cout << "MULHU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (uint32_t)(((uint64_t)((uint32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x2: { // MULHSU
				DEBUG(std::cout << "MULHSU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)(((int64_t)((int32_t)regs[rs1]) * (uint64_t)((uint32_t)regs[rs2])) >> 32);
				break;
			}
			case 0x4: { // DIV
				DEBUG(std::cout << "DIV x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
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
				DEBUG(std::cout << "DIVU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				if (regs[rs2] == 0) {
					result = ((uint64_t)1 << XLEN) - 1;
					break;
				}
				result = regs[rs1] / regs[rs2];
				break;
			}
			case 0x6: { // REM
				DEBUG(std::cout << "REM x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
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
				DEBUG(std::cout << "REMU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
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
					DEBUG(std::cout << "SUB x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)(regs[rs1] - regs[rs2]);
				}
				else {
					DEBUG(std::cout << "ADD x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)(regs[rs1] + regs[rs2]);
				}
				break;
			}
			case 0x2: { // SLT
				DEBUG(std::cout << "SLT x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (int32_t)regs[rs1] < (int32_t)regs[rs2];
				break;
			}
			case 0x3: { // SLTU
				DEBUG(std::cout << "SLTU x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = (uint32_t)regs[rs1] < (uint32_t)regs[rs2];
				break;
			}
			case 0x4: { // XOR
				DEBUG(std::cout << "XOR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] ^ regs[rs2];
				break;
			}
			case 0x6: { // OR
				DEBUG(std::cout << "OR x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] | regs[rs2];
				break;
			}
			case 0x7: { // AND
				DEBUG(std::cout << "AND x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] & regs[rs2];
				break;
			}
			case 0x1: { // SLLI
				DEBUG(std::cout << "SLLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
				result = regs[rs1] << (regs[rs2] & 0x1F);
				break;
			}
			case 0x5: { // SRLI / SRAI
				if (funct7) {
					DEBUG(std::cout << "SRAI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
					result = (int32_t)regs[rs1] >> (int32_t)(regs[rs2] & 0x1F);
				}
				else {
					DEBUG(std::cout << "SRLI x" << rd << ", x" << rs1 << ", x" << rs2 << std::endl);
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
		DEBUG(std::cout << "JAL x" << rd << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
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
		DEBUG(std::cout << "JALR x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
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
			DEBUG(std::cout << "BEQ x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] == regs[rs2];
			break;
		}
		case 0x1: { // BNE
			DEBUG(std::cout << "BNE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] != regs[rs2];
			break;
		}
		case 0x4: { // BLT
			DEBUG(std::cout << "BLT x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = (int32_t)regs[rs1] < (int32_t)regs[rs2];
			break;
		}
		case 0x6: { // BLTU
			DEBUG(std::cout << "BLTU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = regs[rs1] < regs[rs2];
			break;
		}
		case 0x5: { // BGE
			DEBUG(std::cout << "BGE x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
			should_branch = (int32_t)regs[rs1] >= (int32_t)regs[rs2];
			break;
		}
		case 0x7: { // BGEU
			DEBUG(std::cout << "BGEU x" << rs1 << ", x" << rs2 << ", pc" << std::showpos << offset << std::noshowpos << std::endl);
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
			DEBUG(std::cout << "LB x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint8_t result;
			if (read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int8_t)result);
			break;
		}
		case 0x4: { // LBU
			DEBUG(std::cout << "LBU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint8_t result;
			if (read_u8(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x1: { // LH
			DEBUG(std::cout << "LH x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint16_t result;
			if (read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (int32_t)((int16_t)result);
			break;
		}
		case 0x5: { // LHU
			DEBUG(std::cout << "LHU x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
			uint16_t result;
			if (read_u16(regs[rs1] + offset, &result)) {
				// TODO: handle raised exceptions
			}
			if (rd != 0)
				regs[rd] = (uint32_t)result;
			break;
		}
		case 0x2: { // LW
			DEBUG(std::cout << "LW x" << rd << ", x" << rs1 << std::showpos << offset << std::noshowpos << std::endl);
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
			DEBUG(std::cout << "SB x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
			if (write_u8(regs[rs1] + offset, regs[rs2] & 0xFF)) {
				// TODO: handle raised exceptions
			}
			break;
		}
		case 0x1: { // SH
			DEBUG(std::cout << "SH x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
			if (write_u16(regs[rs1] + offset, regs[rs2] & 0xFFFF)) {
				// TODO: handle raised exceptions
			}
			break;
		}
		case 0x2: { // SW
			DEBUG(std::cout << "SW x" << rs1 << std::showpos << offset << std::noshowpos << ", x" << rs2 << std::endl);
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
			DEBUG(std::cout << "FENCE ???" << std::endl);
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
	case 0x73: { // System Instructions + Zicsr Extension
		uint32_t funct3 = (insn >> 12) & 0x7;
		if (funct3) { // Zicsr Instructions
			uint32_t csr = (insn >> 20) & 0xFFF;
			switch (funct3) {
			case 0x1: { // CSRRW
				DEBUG(std::cout << "CSRRW x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
				if (rd != 0) {
					uint32_t old_value = csrs[csr];
					regs[rd] = old_value;
				}
				csrs[csr] = regs[rs1];
				break;
			}
			case 0x2: { // CSRRS
				DEBUG(std::cout << "CSRRS x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
				uint32_t old_value = csrs[csr];
				if (rd != 0) {
					regs[rd] = old_value;
				}
				if (rs1 != 0) {
					csrs[csr] = old_value | regs[rs1];
				}
				break;
			}
			case 0x3: { // CSRRC
				DEBUG(std::cout << "CSRRC x" << rd << ", c" << csr << ", x" << rs1 << std::endl);
				uint32_t old_value = csrs[csr];
				if (rd != 0) {
					regs[rd] = old_value;
				}
				if (rs1 != 0) {
					csrs[csr] = old_value & (~regs[rs1]);
				}
				break;
			}
			case 0x5: { // CSRRWI
				DEBUG(std::cout << "CSRRWI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
				if (rd != 0) {
					uint32_t old_value = csrs[csr];
					regs[rd] = old_value;
				}
				csrs[csr] = rs1; // not actually a register number, a 5 bit zero-extended immediate
				break;
			}
			case 0x6: { // CSRRSI
				DEBUG(std::cout << "CSRRSI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
				uint32_t old_value = csrs[csr];
				if (rd != 0) {
					regs[rd] = old_value;
				}
				if (rs1 != 0) {
					csrs[csr] = old_value | rs1; // not actually a register number, a 5 bit zero-extended immediate
				}
				break;
			}
			case 0x7: { // CSRRCI
				DEBUG(std::cout << "CSRRCI x" << rd << ", c" << csr << ", " << rs1 << std::endl);
				uint32_t old_value = csrs[csr];
				if (rd != 0) {
					regs[rd] = old_value;
				}
				if (rs1 != 0) {
					csrs[csr] = old_value & (~rs1); // not actually a register number, a 5 bit zero-extended immediate
				}
				break;
			}
			default: {
				// TODO: throw illegal instruction exception
				return;
			}
			}
		}
		else { // PRIV
			uint32_t funct12 = (insn >> 20) & 0xFFF;
			if (rd != 0 || rs1 != 0) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (funct12 & 0xFFE) {
				// TODO: throw illegal instruction exception
				return;
			}
			if (funct12) { // EBREAK
				DEBUG(std::cout << "EBREAK" << std::endl);
				// TODO: throw breakpoint exception
				return;
			}
			else { // ECALL
				DEBUG(std::cout << "ECALL" << std::endl);
				// TODO: implement various services
				if (regs[3] & 1) { // system exit
					std::cout << "program exited with code " << (regs[3] >> 1) << std::endl;
					is_running = false;
				}
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

		if (insn == 0) { // TODO: remove this
			is_running = false;
			break;
		}

		if (DEBUG_PRINT)
			print_context();

		interpret_insn(insn);

		pc = next_pc;
	}
}

void RISCV::print_context() {
	for (int i = 0; i < 32; i++) { // print regs
		std::cout << "| x" << i << ": " << regs[i] << "(" << (int32_t)regs[i] << ") ";
	}
	std::cout << "| pc: 0x" << std::setbase(16) << pc << std::setbase(10) << std::endl;
}