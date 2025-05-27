#pragma once

#include "CopySectionPass.h"
#include "ConstantEvaluateUtil.h"

/// 常量折叠优化（浅层次）
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

/// 常量折叠优化（深层次）
class ConstantFoldingDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
