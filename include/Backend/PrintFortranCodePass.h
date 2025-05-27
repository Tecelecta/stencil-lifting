#pragma once

#include "PrintCodePass.h"

class PrintFortranProcedurePass : public PrintProcedurePass
{
public:
	PrintFortranProcedurePass(std::ostream& out = std::cout);

protected:
	std::string getTypeString(Type type) const override;

	std::string getValueNameString(Value* value) const override;

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

	void printLocalValues(Section* section, uint32_t i);

private:
	std::vector<uint32_t> printConstantValue;
};

class PrintFortranModulePass : public PrintModulePass
{
public:
	PrintFortranModulePass(std::ostream& out = std::cout) : PrintModulePass(out) {}

	BACKEND_API void run(GraphValueProjection src, const std::vector<Value*>& returnValues, std::string_view moduleName);

protected:
	BACKEND_API void runOnSubgraph(uint32_t i) override;
};
