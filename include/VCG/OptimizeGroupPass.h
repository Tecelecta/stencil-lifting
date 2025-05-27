#pragma once

#include "CopySectionPass.h"

typedef std::vector<std::pair<Value*, Integer>> GroupNormalForm;

/**
	* @brief 代数系统：群或半群，加法运算
	* @details
	* 定义该代数系统上运算满足结合律，可选满足交换律，可选具有单位元和逆元。
	*/
struct GroupAlgebraSystem
{
	Type elemType; //!< 定义代数系统的元素类型
	Operation addOp; //!< 定义代数系统的“加法”运算
	Operation subOp; //!< 定义代数系统的“减法”运算
	Operation negOp; //!< 定义代数系统的“相反数”运算
	Operation scaOp; //!< 定义代数系统的“数乘”运算
	ConstantValue* zero = nullptr; //!< 定义代数系统的“0”
	bool isAbelian = false; //!< 是否阿贝尔群（交换律）
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

/// 提取标准化的群代数运算（浅层次）
class ExtractGroupShallowPass : public CopySectionShallowPass, public AnalyzeGroupPass
{
public:
	VCG_API ExtractGroupShallowPass();
	DEFAULT_COPY_MOVE(ExtractGroupShallowPass)

	VCG_API GraphOuterSection run(GraphOuterSection src) override;

	static Value* createTerm(Context* context, const CopySectionShallowPass& pass, Value* term, Integer coef,
		OperationValue::Builder& builder, Operation negOp, Operation scaOp);
};

/// 提取标准化的群代数运算（深层次）
class ExtractGroupDeepPass : public CopySectionDeepPass, public GroupAlgebraSystem
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
