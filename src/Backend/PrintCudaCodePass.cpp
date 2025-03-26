#define BACKEND_EXPORTS
#include "PrintCudaCodePass.h"

void PrintFortranCudaProcedurePass::printFunctionHead(Section* section, uint32_t i)
{
	if (isKernel(section))
	{
		out << "module kernel_" << parent->subgraphId(section) << "\ncontains\n\n";
		printTabs(tabNum);
		out << "attributes(global) ";
	}
	PrintFortranProcedurePass::printFunctionHead(section, i);
}

void PrintFortranCudaProcedurePass::printFunctionTail(Section* section, uint32_t i)
{
	PrintFortranProcedurePass::printFunctionTail(section, i);
	if (isKernel(section))
	{
		out << "end module\n\n";
	}
}

void PrintFortranCudaProcedurePass::printFunctionCall(SectionCall* call, uint32_t i)
{
	printTabs(tabNum);
	out << "call ";
	out << getSectionNameString(call->getCallee());
	if (dynamic_cast<ParallelLoop*>(call->getCaller()) != nullptr)
	{
		out << "<<<grid_dim, blk_dim>>>";
	}
	out << '(';
	auto args(call->getArgumentVector());
	args.insert(args.end(), call->getSpecializerVector().begin(), call->getSpecializerVector().end());
	for (uint32_t j = 0; j < args.size(); j++)
	{
		out << printVertexVector[vertexId(args[j])].name;
		if (j < args.size() - 1)
		{
			out << ", ";
		}
	}
	out << ")\n";
}

void PrintFortranCudaProcedurePass::printParallelLoopBegin(ParallelLoop* loop, uint32_t i)
{
	if (loop->getCounterNum() > 3)
	{
		return PrintFortranProcedurePass::printParallelLoopBegin(loop, i);
	}
	std::string blockDim_x = "1";
	std::string blockDim_y = "1";
	std::string blockDim_z = "1";
	std::string gridDim_x = "1";
	std::string gridDim_y = "1";
	std::string gridDim_z = "1";
	auto str_array = getValueNameString(loop->getPhiValue(0));
	switch (loop->getCounterNum())
	{
	case 1:
		blockDim_x = "128";
		gridDim_x = std::string("(SIZE(") + str_array + ", 1) + 127) / 128";
		break;
	case 2:
		blockDim_x = "16";
		gridDim_x = std::string("(SIZE(") + str_array + ", 1) + 15) / 16";
		blockDim_y = "8";
		gridDim_y = std::string("(SIZE(") + str_array + ", 2) + 7) / 8";
		break;
	case 3:
		blockDim_y = "8";
		gridDim_x = std::string("(SIZE(") + str_array + ", 1) + 7) / 8";
		blockDim_y = "4";
		gridDim_y = std::string("(SIZE(") + str_array + ", 2) + 3) / 4";
		blockDim_y = "4";
		gridDim_z = std::string("(SIZE(") + str_array + ", 3) + 3) / 4";
		break;
	}
	printTabs(tabNum);
	out << "type(dim3) :: grid_dim, block_dim\n";
	printTabs(tabNum);
	out << "grid_dim = dim3(" << gridDim_x << ", " << gridDim_y << ", " << gridDim_z << '\n';
	printTabs(tabNum);
	out << "block_dim = dim3(" << blockDim_x << ", " << blockDim_y << ", " << blockDim_z << '\n';
}

void PrintFortranCudaProcedurePass::printParallelLoopEnd(ParallelLoop* loop, uint32_t i)
{
	if (loop->getCounterNum() > 3)
	{
		return PrintFortranProcedurePass::printParallelLoopEnd(loop, i);
	}
}

bool PrintFortranCudaProcedurePass::isKernel(Section* section) const
{
	const auto& callers = parent->getCallerBySubgraph(parent->subgraphId(section));
	auto iter = std::find_if(callers.begin(), callers.end(), [this](uint32_t i) {
			return dynamic_cast<ParallelLoop*>(parent->subgraphAt(i)) != nullptr;
		});
	return iter != callers.end();
}

void PrintFortranCudaModulePass::runOnSubgraph(uint32_t i)
{
	PrintFortranCudaProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}

void PrintCppCudaModulePass::runOnSubgraph(uint32_t i)
{
	PrintCppCudaProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}
