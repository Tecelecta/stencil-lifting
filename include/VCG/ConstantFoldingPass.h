#pragma once

#include "CopySectionPass.h"
#include "ConstantEvaluateUtil.h"

/// 
class ConstantFoldingShallowPass : public CopySectionShallowPass, protected ConstantEvaluateUtil, private SccAction
{
public:
	VCG_API ConstantFoldingShallowPass();
	DEFAULT_COPY_MOVE(ConstantFoldingShallowPass)

protected:
	void createValueBuilders() override;

private:
	bool runOnTrivial(uint32_t vertex) override;

	bool runOnTypical(const vertex_list& scc) override;
};

/// 
class ConstantFoldingDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
