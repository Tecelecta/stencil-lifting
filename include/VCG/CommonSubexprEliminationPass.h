#pragma once

#define VCG_EXPORTS

#include "CopySectionPass.h"

class AnalyzeCommonSubexprPass : virtual public Pass<GraphOuterSection>,
	private Value::Visitor<uint32_t, uint32_t>, private SccAction
{
public:
	VCG_API AnalyzeCommonSubexprPass();
	DEFAULT_COPY_MOVE(AnalyzeCommonSubexprPass)

	void run();

private:
	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

private:
	class OperationValueHasher
	{
	public:
		OperationValueHasher(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		size_t operator()(uint32_t a) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class OperationValueEqualTo
	{
	public:
		OperationValueEqualTo(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		bool operator()(uint32_t a, uint32_t b) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class ResultValueHasher
	{
	public:
		ResultValueHasher(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		size_t operator()(uint32_t a) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class ResultValueEqualTo
	{
	public:
		ResultValueEqualTo(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		bool operator()(uint32_t a, uint32_t b) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class PhiValueHasher
	{
	public:
		PhiValueHasher(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		size_t operator()(uint32_t a) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class PhiValueEqualTo
	{
	public:
		PhiValueEqualTo(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		bool operator()(uint32_t a, uint32_t b) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class ConstantValueHasher
	{
	public:
		ConstantValueHasher(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		size_t operator()(uint32_t a) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class ConstantValueEqualTo
	{
	public:
		ConstantValueEqualTo(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		VCG_API bool operator()(uint32_t a, uint32_t b) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class InvalidValueHasher
	{
	public:
		InvalidValueHasher(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		size_t operator()(uint32_t a) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	class InvalidValueEqualTo
	{
	public:
		InvalidValueEqualTo(AnalyzeCommonSubexprPass& pass) : pass(pass) {}

		bool operator()(uint32_t a, uint32_t b) const;

	private:
		AnalyzeCommonSubexprPass& pass;
	};

	template<typename T>
	uint32_t findOrReturnSelf(Value* value, uint32_t i, T& equSet)
	{
		auto iter = equSet.find(i);
		if (iter == equSet.end())
		{
			iter = equSet.insert(i).first;
		}
		return *iter;
	}

private:
	std::unordered_set<uint32_t, OperationValueHasher, OperationValueEqualTo> equSetOperationValue;
	std::unordered_set<uint32_t, ResultValueHasher, ResultValueEqualTo> equSetResultValue;
	std::unordered_set<uint32_t, PhiValueHasher, PhiValueEqualTo> equSetPhiValue;
	std::unordered_set<uint32_t, ConstantValueHasher, ConstantValueEqualTo> equSetConstantValue;
	std::unordered_set<uint32_t, InvalidValueHasher, InvalidValueEqualTo> equSetInvalidValue;
};

/// 公共子表达式删除优化（浅层次）
class CommonSubexprEliminationShallowPass : public CopySectionShallowPass, protected AnalyzeCommonSubexprPass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src) override;
};

/// 公共子表达式删除优化（深层次）
class CommonSubexprEliminationDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
