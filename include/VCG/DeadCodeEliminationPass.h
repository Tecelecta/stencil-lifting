#pragma once

#include "CopySectionPass.h"

/// 
class DeadCodeEliminationShallowPass : public CopySectionShallowPass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src, const vertex_list& initUsefulList);
};

/// 
class DeadCodeEliminationDeepPass : public CopySectionDeepPass
{
public:
	VCG_API GraphValueProjection run(GraphValueProjection src, const vertex_list& initUsefulList);

protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
