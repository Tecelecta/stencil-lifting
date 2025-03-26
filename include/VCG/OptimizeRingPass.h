#pragma once

#include "OptimizeGroupPass.h"

typedef std::vector<std::pair<GroupNormalForm, Integer>> RingNormalForm;

/**
	* @brief 代数系统：环
	* @details
	* 定义该代数系统上运算满足结合律，可选满足交换律，可选具有单位元和逆元。
	*/
struct RingAlgebraSystem
{
	Type elemType; //!< 定义代数系统的元素类型
	Operation addOp; //!< 定义代数系统的“加法”运算
	Operation subOp; //!< 定义代数系统的“减法”运算
	Operation negOp; //!< 定义代数系统的“相反数”运算
	Operation mulOp; //!< 定义代数系统的“乘法”运算
	Operation scaOp; //!< 定义代数系统的“数乘”运算
	Operation powOp; //!< 定义代数系统的“整数次幂”运算
	ConstantValue* zero = nullptr; //!< 定义代数系统的“0”
	ConstantValue* one = nullptr; //!< 定义代数系统的“1”
	bool isCommutative = false; //!< 是否交换环
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

/// 提取标准化的环代数运算（浅层次）
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

/// 提取标准化的环代数运算（深层次）
class ExtractRingDeepPass : public CopySectionDeepPass, public RingAlgebraSystem
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
