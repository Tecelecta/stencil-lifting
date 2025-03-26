#pragma once

#include "PassBase.h"
#include "ArrayAccessVisitor.h"

#include <set>

class AnalyzeArrayBasePass : virtual public Pass<GraphValueProjection>,
	protected SccAction, private ArrayAccessVisitor
{
protected:
	VCG_API void run();

	VCG_API bool runOnTrivial(uint32_t vertex) override;

	VCG_API bool runOnTypical(const vertex_list& scc) override;

private:
	VCG_API void visitArrayParameter(InputValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArrayResult(ResultValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArrayCopyOp(OperationValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArraySelectOp(OperationValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArraySetOp(OperationValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArrayPhi(PhiValue* value, uint32_t dst_vertex) override;
	VCG_API void visitArrayConstant(ConstantValue* value, uint32_t dst_vertex) override;

public:
	std::vector<std::set<uint32_t>> arrayBaseSetVector;
	std::vector<uint32_t> arrayBaseVector;
};
