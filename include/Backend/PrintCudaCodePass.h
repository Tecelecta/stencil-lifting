#pragma once

#include "PrintFortranCodePass.h"
#include "PrintCppCodePass.h"

class PrintFortranCudaProcedurePass : public PrintFortranProcedurePass
{
public:
	PrintFortranCudaProcedurePass(std::ostream& out = std::cout) : PrintFortranProcedurePass(out) {}

	void printFunctionHead(Section* section, uint32_t i) override;

	void printFunctionTail(Section* section, uint32_t i) override;

	void printFunctionCall(SectionCall* call, uint32_t i) override;

	void printParallelLoopBegin(ParallelLoop* loop, uint32_t i) override;

	void printParallelLoopEnd(ParallelLoop* loop, uint32_t i) override;

protected:
	bool isKernel(Section* section) const;
};

class PrintFortranCudaModulePass : public PrintFortranModulePass
{
public:
	PrintFortranCudaModulePass(std::ostream& out = std::cout) : PrintFortranModulePass(out) {}

protected:
	BACKEND_API void runOnSubgraph(uint32_t i) override;
};

class PrintCppCudaProcedurePass : public PrintCppProcedurePass
{
public:
	PrintCppCudaProcedurePass(std::ostream& out = std::cout) : PrintCppProcedurePass(out) {}
};

class PrintCppCudaModulePass : public PrintCppModulePass
{
public:
	PrintCppCudaModulePass(std::ostream& out = std::cout) : PrintCppModulePass(out) {}

protected:
	BACKEND_API void runOnSubgraph(uint32_t i) override;
};
