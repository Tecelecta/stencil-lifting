#define BACKEND_EXPORTS
#include "PrintCppCodePass.h"

void PrintCppModulePass::runOnSubgraph(uint32_t i)
{
	PrintCppProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}
