#pragma once

#include "OptimizeGroupPass.h"

typedef std::vector<std::pair<GroupNormalForm, Integer>> RingNormalForm;

/**
	* @brief 
	* @details
	* 
	*/
struct RingAlgebraSystem
{
	Type elemType; //!< 
	Operation addOp; //!< 
	Operation subOp; //!< 
	Operation negOp; //!< 
	Operation mulOp; //!< 
	Operation scaOp; //!< 
	Operation powOp; //!< 
	ConstantValue* zero = nullptr; //!< 0
	ConstantValue* one = nullptr; //!< 1
	bool isCommutative = false; //!< 
};

class AnalyzeRingPass : virtual public Pass<GraphOuterSection>, public RingAlgebraSystem,
	private Value::Visitor<void, uint32_t>, private SccAction
{
public:
	VCG_API AnalyzeRingPass();
	DEFAULT_COPY_MOVE(AnalyzeRingPass)

	void run();

	void mergeSameElement(RingNormalForm& ring, const GraphNoCallPath& graph);

private:
	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

public:
	std::vector<RingNormalForm> ringNormalForms;
};

/// 
class ExtractRingShallowPass : public CopySectionShallowPass, public AnalyzeRingPass
{
public:
	VCG_API ExtractRingShallowPass();
	DEFAULT_COPY_MOVE(ExtractRingShallowPass)

	VCG_API GraphOuterSection run(GraphOuterSection src) override;

	static Value* createFactor(Context* context, const CopySectionShallowPass& pass, Value* factor, Integer exponent,
		OperationValue::Builder& builder, Operation powOp);

private:
	Value* createTerm(Context* context, SimpleSection::Builder& simpleSectionBuilder, const GroupNormalForm& term, Integer coef,
		OperationValue::Builder& builder, Operation powOp, Operation mulOp, Operation negOp, Operation scaOp);
};

/// 
class ExtractRingDeepPass : public CopySectionDeepPass, public RingAlgebraSystem
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
