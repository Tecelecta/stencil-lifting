﻿#define VCG_EXPORTS
#include "ConstantFoldingPass.h"
#include "Context.h"

ConstantFoldingShallowPass::ConstantFoldingShallowPass()
{
	createValueBuildersVisitor.visitOperationValue = [this](OperationValue* value, uint32_t i)
		{
			if (value->getOperation().getName().equals("select"))
			{
				if (auto selector = dynamic_cast<ConstantValue*>(mapValue(value->getSrc(0))))
				{
					// 常量选择子
					valueResultVector[i] = mapValue(value->getSrc(getSelectedOperand(selector)));
					return;
				}
				valueBuilderVector[i] = value->cloneBuilder();
				valueResultVector[i] = valueBuilderVector[i].getInstance();
				return;
			}
			std::vector<ConstantValue*> srcVector_numeral;
			for (auto src : value->getSrcVector())
			{
				if (auto src_const = dynamic_cast<ConstantValue*>(mapValue(src)))
				{
					srcVector_numeral.push_back(src_const);
				}
				else
				{
					valueBuilderVector[i] = value->cloneBuilder();
					valueResultVector[i] = valueBuilderVector[i].getInstance();
					return;
				}
			}
			// 折叠常量表达式
			valueResultVector[i] = tryEvaluate(value->getContext(), value->getOperation(), value->getName(), srcVector_numeral);
			if (valueResultVector[i] == nullptr)
			{
				createValueBuildersVisitor.visitVariableValue(value, i);
			}
		};
}

void ConstantFoldingShallowPass::createValueBuilders()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	valueBuilderVector.resize(getVertexNum());
	iterateSCC(*this);
}

bool ConstantFoldingShallowPass::runOnTrivial(uint32_t i)
{
	if (vertexFilter.empty() || vertexFilter.at(i))
	{
		createValueBuildersVisitor(vertexAt(i), i);
	}
	return true;
}

bool ConstantFoldingShallowPass::runOnTypical(const vertex_list& scc)
{
	// 常量表达式不可能构成非平凡强连通分量
	// 常量次数的循环先进行循环展开
	for (auto i : scc)
	{
		if (vertexFilter.empty() || vertexFilter.at(i))
		{
			createValueBuildersVisitor(vertexAt(i), i);
		}
	}
	return true;
}

void ConstantFoldingDeepPass::runOnSubgraph(uint32_t i)
{
	ConstantFoldingShallowPass subPass;
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
	updateResultVector(subPass);
}
