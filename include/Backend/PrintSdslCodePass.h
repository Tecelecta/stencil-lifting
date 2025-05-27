#pragma once

#include "PrintCodePass.h"

class PrintSdslProcedurePass : public PrintProcedurePass
{
public:
	PrintSdslProcedurePass(std::ostream& out = std::cout);

protected:
	std::string getExpr(uint32_t i) const override;

	std::string getNumeralExpr(uint32_t i) const override;

	std::string getArrayGetExpr(uint32_t i) const override;

	std::string getArraySetExpr(uint32_t i) const override;

	void printFunctionHead(Section* section, uint32_t i) override;

	void printFunctionTail(Section* section, uint32_t i) override;

	void printFunctionCall(SectionCall* call, uint32_t i) override;

	void printBinaryBranchBegin(BinaryBranch* branch, uint32_t i) override;

	void printBinaryBranchElse(BinaryBranch* branch, uint32_t i) override;

	void printBinaryBranchEnd(BinaryBranch* branch, uint32_t i) override;

	void printIterateLoopBegin(IterateLoop* loop, uint32_t i) override;

	void printIterateLoopEnd(IterateLoop* loop, uint32_t i) override;

	void printParallelLoopBegin(ParallelLoop* loop, uint32_t i) override;

	void printParallelLoopEnd(ParallelLoop* loop, uint32_t i) override;
};

class PrintSdslModulePass : public PrintModulePass
{
public:
	PrintSdslModulePass(std::ostream& out = std::cout) : PrintModulePass(out) {}

protected:
	BACKEND_API void runOnSubgraph(uint32_t i) override;
};
