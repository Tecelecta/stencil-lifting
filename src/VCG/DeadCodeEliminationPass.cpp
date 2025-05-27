#define VCG_EXPORTS

#include "DeadCodeEliminationPass.h"

GraphOuterSection DeadCodeEliminationShallowPass::run(GraphOuterSection src, const vertex_list& initUsefulList)
{
	setGraph(std::move(src));
	createFilter(vertexFilter, subgraphFilter, initUsefulList);
	CopySectionShallowPass::run();
	return std::move(result);
}

GraphValueProjection DeadCodeEliminationDeepPass::run(GraphValueProjection src, const vertex_list& initUsefulList)
{
	setGraph(std::move(src));
	createFilter(vertexFilter, subgraphFilter, initUsefulList);
	CopySectionDeepPass::run();
	return std::move(result);
}

void DeadCodeEliminationDeepPass::runOnSubgraph(uint32_t i)
{
	CopySectionShallowPass subPass;
	subPass.parent = this;
	subPass.load({ subgraphAt(i) });
	subPass.vertexFilter.resize(subPass.getVertexNum());
	for (uint32_t j = 0; j < subPass.getVertexNum(); j++)
	{
		auto value = subPass.vertexAt(j);
		if (vertexFilter.at(vertexId(value)))
		{
			subPass.vertexFilter[j] = true;
		}
	}
	subPass.run();
	updateResultVector(subPass);
}
