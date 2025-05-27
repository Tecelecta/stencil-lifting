#define VCG_EXPORTS

#include "Context.h"
#include "Optimize.h"
#include "DeadCodeEliminationPass.h"
#include "CopyPropagationPass.h"
#include "CommonSubexprEliminationPass.h"
#include "ConstantFoldingPass.h"
#include "SimpleSectionInlinePass.h"
#include "StencilLiftingPass.h"
#include "OptimizeRingPass.h"
#include "ExtractLoopInvarPass.h"

GraphValueProjection runOptimizeLevel1(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols)
{
	vertex_list initUsefulList;
	for (const auto& section : graph.getSubgraphVector())
	{
		if (initUsefulSymbols.count(section->getName()) > 0)
		{
			auto vars = section->getVariableVector();
			for (auto value : vars)
			{
				if (initUsefulSymbols.count(value->getName()) > 0)
				{
					if (dynamic_cast<OperationValue*>(value) != nullptr || dynamic_cast<ResultValue*>(value) != nullptr)
					{
						initUsefulList.push_back(graph.vertexId(value));
					}
				}
			}
		}
	}
	graph = DeadCodeEliminationDeepPass().run(std::move(graph), initUsefulList);
	graph = CopyPropagationDeepPass().run(std::move(graph));
	graph = CommonSubexprEliminationDeepPass().run(std::move(graph));
	graph = ConstantFoldingDeepPass().run(std::move(graph));
	graph.validate();
	printf("SubgraphNum = %u, VertexNum = %u\n", graph.getSubgraphNum(), graph.getVertexNum());
	return graph;
}

GraphValueProjection runOptimizeLevel2(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols)
{
	graph = runOptimizeLevel1(graph, initUsefulSymbols);

	/*auto context = graph.getRootSection(0)->getContext();

	ExtractRingDeepPass extractRingDeepPass;
	extractRingDeepPass.elemType = context->getRealType();
	extractRingDeepPass.addOp = context->getOperation(context->getString("Real.add"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractRingDeepPass.subOp = context->getOperation(context->getString("Real.sub"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractRingDeepPass.negOp = context->getOperation(context->getString("Real.neg"), { context->getRealType() }, context->getRealType());
	extractRingDeepPass.mulOp = context->getOperation(context->getString("Real.mul"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractRingDeepPass.scaOp = context->getOperation(context->getString("Real.mul"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractRingDeepPass.powOp = context->getOperation(context->getString("Real.pow"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractRingDeepPass.zero = context->getZeroValue();
	extractRingDeepPass.one = context->createConstantValue(context->getIntegerType(), context->getInteger(1));
	extractRingDeepPass.isCommutative = true;
	graph = extractRingDeepPass.run(graph);

	ExtractGroupDeepPass extractGroupDeepPass;
	extractGroupDeepPass.elemType = context->getRealType();
	extractGroupDeepPass.addOp = context->getOperation(context->getString("Real.add"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractGroupDeepPass.subOp = context->getOperation(context->getString("Real.sub"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractGroupDeepPass.negOp = context->getOperation(context->getString("Real.neg"), { context->getRealType() }, context->getRealType());
	extractGroupDeepPass.scaOp = context->getOperation(context->getString("Real.mul"), { context->getRealType(), context->getRealType() }, context->getRealType());
	extractGroupDeepPass.zero = context->getZeroValue();
	extractGroupDeepPass.isAbelian = true;
	graph = extractGroupDeepPass.run(graph);
	*/
	//graph = ExtractLoopInvarDeepPass().run(graph);
	graph = SimpleSectionInlineDeepPass().run(graph);
	graph.validate();
	printf("SubgraphNum = %u, VertexNum = %u\n", graph.getSubgraphNum(), graph.getVertexNum());
	graph = runOptimizeLevel1(graph, initUsefulSymbols); 
	return graph;
}

GraphValueProjection runOptimizeLevel3(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols)
{
	graph = runOptimizeLevel2(graph, initUsefulSymbols);
	graph = StencilLiftingPass().run(graph);
	graph.validate();
	printf("SubgraphNum = %u, VertexNum = %u\n", graph.getSubgraphNum(), graph.getVertexNum());
	graph = runOptimizeLevel2(graph, initUsefulSymbols);
	return graph;
}
