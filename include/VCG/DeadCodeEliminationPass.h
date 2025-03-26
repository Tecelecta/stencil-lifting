#pragma once

#include "CopySectionPass.h"

/// 无用代码删除优化（浅层次）
class DeadCodeEliminationShallowPass : public CopySectionShallowPass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src, const vertex_list& initUsefulList);
};

/// 无用代码删除优化（深层次）
class DeadCodeEliminationDeepPass : public CopySectionDeepPass
{
public:
	VCG_API GraphValueProjection run(GraphValueProjection src, const vertex_list& initUsefulList);

protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
