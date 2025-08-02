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

/// 
class CopyPropagationShallowPass : public CopySectionShallowPass, protected AnalyzeCopyValuePass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src) override;
};

/// 
class CopyPropagationDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
