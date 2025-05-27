#pragma once

#define VCG_EXPORTS

#include "CopySectionPass.h"

class AnalyzeCopyValuePass : virtual public Pass<GraphOuterSection>,
	private Value::Visitor<uint32_t, uint32_t>, private SccAction
{
public:
	VCG_API AnalyzeCopyValuePass();
	DEFAULT_COPY_MOVE(AnalyzeCopyValuePass)

	void run();

private:
	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;
};

/// 复制传播优化（浅层次）
class CopyPropagationShallowPass : public CopySectionShallowPass, protected AnalyzeCopyValuePass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src) override;
};

/// 复制传播优化（深层次）
class CopyPropagationDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
