#define VCG_EXPORTS

#include "StencilLiftingPass.h"
#include "SummaryIterativeSearcher.h"
#include "DeadCodeEliminationPass.h"

#include <ctime>

GraphValueProjection StencilLiftingPass::run(GraphValueProjection src)
{
	setGraph(std::move(src));
	auto t0 = clock();
	CopySectionDeepPass::run();		
	std::cout << "用时" << double(clock() - t0) / CLOCKS_PER_SEC << "秒" << std::endl;
	return std::move(result);
}

void StencilLiftingPass::runOnSubgraph(uint32_t i)
{
	CopySectionDeepPass::runOnSubgraph(i);
	auto section = subgraphAt(i);
	for (auto call : section->getSectionCallVector())
	{
		if (auto loop = dynamic_cast<IterateLoop*>(call->getCallee()))
		{
			SummaryIterativeSearcher subPass;
			subPass.parent = this;
			subPass.loadLoop(dynamic_cast<IterateLoop*>(mapSection(loop)));
			for (size_t i = 0; i < call->getSpecializerVectorSize(); i++)
			{
				if (auto constant = dynamic_cast<ConstantValue*>(call->getSpecializer(i)))
				{
					subPass.constantSpecializer[i] = constant->getValue<Integer>();
				}
			}
			subPass.run();
			sectionResultVector[subgraphId(loop)] = subPass.sectionResultVector[1];
			auto varVector = loop->getVariableVector();
			for (auto var : varVector)
			{
				valueResultVector[vertexId(var)] = subPass.mapValue(mapValue(var));
			}
		}
	}
	CopySectionDeepPass::runOnSubgraph(i);
}
