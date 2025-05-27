#pragma once

#include "PrintCodePass.h"

class PrintCppProcedurePass : public PrintProcedurePass
{
public:
	PrintCppProcedurePass(std::ostream& out = std::cout) : PrintProcedurePass(out) {}
};

class PrintCppModulePass : public PrintModulePass
{
public:
	PrintCppModulePass(std::ostream& out = std::cout) : PrintModulePass(out) {}

protected:
	BACKEND_API void runOnSubgraph(uint32_t i) override;
};
