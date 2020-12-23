#include <iostream>
#include <memory>
#include "RISCV.h"

int main(int argc, char** argv) {
	if (argc < 2) {
		std::cerr << "Invalid Usage! Try: " << argv[0] << " <program file> [entrypoint address] [base address]" << std::endl;
		return 1;
	}

	uint32_t entrypoint = 0x0;
	if (argc >= 3) {
		entrypoint = std::stoi(argv[2], nullptr, 0);
	}

	uint32_t mem_start = 0x0;
	if (argc >= 4) {
		mem_start = std::stoi(argv[3], nullptr, 0);
	}

	try {
		std::unique_ptr<RISCV> hart = std::make_unique<RISCV>(argv[1], mem_start, entrypoint);
		hart->run();
	}
	catch (std::runtime_error& err) {
		std::cout << "ERROR: " << err.what() << std::endl;
	}
}
