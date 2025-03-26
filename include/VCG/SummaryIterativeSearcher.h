#pragma once

#include "CopySectionPass.h"
#include "SectionToZ3Pass.h"
#include "LoopParallelizePass.h"

class SummaryIterativeSearcher final : public CopyLoopAndBodyPass,
	private SccAction, private Value::Visitor<bool, uint32_t>
{
public:
	SummaryIterativeSearcher();

protected:
	void runOnBody() override;

	void runOnLoop() override;

private:
	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

	Summary updateSummary(const Summary& original, std::unordered_map<Z3_ast, Summary>& summaryMap, bool& converged);

	struct SelectSubstituteAction
	{
		z3::expr cond;
		z3::expr src;
		z3::expr dst;
	};

	bool doSelectSubstitute(std::vector<Summary::Branch>& dst_branch, const Summary::Branch& src_branch);

	std::vector<SelectSubstituteAction> getSelectSubstitute(z3::expr cond, z3::expr elem, z3::expr r, std::unordered_set<Z3_ast>& visited);

	Summary propagateRead(const Summary& summary);

	bool getReadSubstitute(z3::expr_vector& src, z3::expr_vector& dst, z3::expr elem, std::unordered_set<Z3_ast>& visited);

	Summary nextTimeStep(const Summary& invar);

	Summary propagateSkolemX(const Summary& invar);

	Summary propagateSkolemB(const Summary& invar);

	Summary getPostCond(const Summary& invar);

private:
	z3::context z3ctx;
	z3::expr t;
	z3::expr x;
	z3::expr t_begin;
	z3::expr t_times;
	z3::expr t_step;
	z3::expr_vector v_t;
	z3::expr_vector v_x;
	z3::expr_vector dst_t;
	SectionToZ3Pass z3Pass;
	std::unordered_map<Z3_ast, Summary> summaryMap;
	LoopParallelizePass parallelizePass;
};
