#pragma once

#include "CopySectionPass.h"

/// Stencil计算的提升
class StencilLiftingPass : public CopySectionDeepPass
{
public:
	VCG_API GraphValueProjection run(GraphValueProjection src) override;

protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
