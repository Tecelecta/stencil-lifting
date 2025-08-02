#pragma once

#include "CopySectionPass.h"

typedef std::vector<std::pair<Value*, Integer>> GroupNormalForm;

/**
	* @brief 
	* @details
	* 
	*/
struct GroupAlgebraSystem
{
	Type elemType; //!< 
	Operation addOp; //!< 
	Operation subOp; //!< 
	Operation negOp; //!< 
	Operation scaOp; //!< 
	ConstantValue* zero = nullptr; //!< 0
	bool isAbelian = false; //!< 
};

class AnalyzeGroupPass : virtual public Pass<GraphOuterSection>, public GroupAlgebraSystem,
	private Value::Visitor<void, uint32_t>, private SccAction
{
public:
	VCG_API AnalyzeGroupPass();
	DEFAULT_COPY_MOVE(AnalyzeGroupPass)

	void run();

	static void mergeSameElement(GroupNormalForm& group, const GraphNoCallPath& graph);

	static void mergeAdjacentSameElement(GroupNormalForm& group);

private:
	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

public:
	std::vector<GroupNormalForm> groupNormalForms;
};

/// 
class ExtractGroupShallowPass : public CopySectionShallowPass, public AnalyzeGroupPass
{
public:
	VCG_API ExtractGroupShallowPass();
	DEFAULT_COPY_MOVE(ExtractGroupShallowPass)

	VCG_API GraphOuterSection run(GraphOuterSection src) override;

	static Value* createTerm(Context* context, const CopySectionShallowPass& pass, Value* term, Integer coef,
		OperationValue::Builder& builder, Operation negOp, Operation scaOp);
};

/// 
class ExtractGroupDeepPass : public CopySectionDeepPass, public GroupAlgebraSystem
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
