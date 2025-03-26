#define VCG_EXPORTS
#include "AnalyzeArrayBasePass.h"

void AnalyzeArrayBasePass::run()
{
	arrayBaseSetVector.resize(getVertexNum());
	arrayBaseVector.resize(getVertexNum());
	iterateSCC(*this);
	for (uint32_t i = 0; i < getVertexNum(); i++)
	{
		if (arrayBaseSetVector[i].size() == 1)
		{
			arrayBaseVector[i] = *arrayBaseSetVector[i].begin();
		}
		else
		{
			arrayBaseVector[i] = i;
		}
	}
}

bool AnalyzeArrayBasePass::runOnTrivial(uint32_t vertex)
{
	auto value = vertexAt(vertex);
	arrayBaseSetVector[vertex] = {};
	ArrayAccessVisitor::operator()(value, vertex);
	return true;
}

bool AnalyzeArrayBasePass::runOnTypical(const vertex_list& scc)
{
	bool changed;
	do {
		changed = false;
		for (uint32_t i : scc)
		{
			auto saved = std::move(arrayBaseSetVector[i]);
			arrayBaseSetVector[i] = {};
			ArrayAccessVisitor::operator()(vertexAt(i), i);
			changed = changed || saved != arrayBaseSetVector[i];
		}
	} while (changed);
	return true;
}

void AnalyzeArrayBasePass::visitArrayParameter(InputValue* value, uint32_t dst_vertex)
{
	arrayBaseSetVector[dst_vertex] = { vertexId(value) };
}

void AnalyzeArrayBasePass::visitArrayResult(ResultValue* value, uint32_t dst_vertex)
{
	auto src_vertex = vertexId(value->getSrc());
	for (auto i : arrayBaseSetVector[src_vertex])
	{
		for (auto j : dependencyVector[i])
		{
			arrayBaseSetVector[dst_vertex].insert(arrayBaseSetVector[j].begin(), arrayBaseSetVector[j].end());
		}
	}
}

void AnalyzeArrayBasePass::visitArrayCopyOp(OperationValue* value, uint32_t dst_vertex)
{
	arrayBaseSetVector[dst_vertex] = arrayBaseSetVector[vertexId(value->getSrc(0))];
}

void AnalyzeArrayBasePass::visitArraySelectOp(OperationValue* value, uint32_t dst_vertex)
{
	const auto& src1 = arrayBaseSetVector[vertexId(value->getSrc(1))];
	const auto& src2 = arrayBaseSetVector[vertexId(value->getSrc(2))];
	arrayBaseSetVector[dst_vertex].insert(src1.begin(), src1.end());
	arrayBaseSetVector[dst_vertex].insert(src2.begin(), src2.end());
}

void AnalyzeArrayBasePass::visitArraySetOp(OperationValue* value, uint32_t dst_vertex)
{
	arrayBaseSetVector[dst_vertex] = arrayBaseSetVector[vertexId(value->getSrc(0))];
}

void AnalyzeArrayBasePass::visitArrayPhi(PhiValue* value, uint32_t dst_vertex)
{
	for (auto i : dependencyVector[dst_vertex])
	{
		arrayBaseSetVector[dst_vertex].insert(arrayBaseSetVector[i].begin(), arrayBaseSetVector[i].end());
	}
}

void AnalyzeArrayBasePass::visitArrayConstant(ConstantValue* value, uint32_t dst_vertex)
{
	arrayBaseSetVector[dst_vertex] = { vertexId(value) };
}
