#pragma once

#include "CopySectionPass.h"
#include "TypeToZ3Visitor.h"
#include "Summary.h"

class LoopParallelizePass : public CopySectionShallowPass
{
public:
	LoopParallelizePass(z3::context& z3ctx);

protected:
	void createValueBuilders() override;

	void createSectionBuilders() override;

private:
	template<typename SectionBuilder>
	struct BuilderInfo
	{
		SectionBuilder sectionBuilder;
		SectionCall::Builder callBuilder;
		std::unordered_map<Z3_ast, Value*> valueMap;
	};

	struct SummaryBranchBuilders
	{
		BuilderInfo<BinaryBranch::Builder> branch;
		BuilderInfo<SimpleSection::Builder> leaf;
	};

	struct SummaryBuilders
	{
		BuilderInfo<ParallelLoop::Builder> loop;
		BuilderInfo<SimpleSection::Builder> body;
		std::vector<SummaryBranchBuilders> branches;
	};

	PhiValue* translatePhi(PhiValue* value, const Summary& summary, SummaryBuilders& summaryBuilders);

	Value* translateExprAndSaveResult(z3::expr expr, SimpleSection::Builder builder,
		std::unordered_map<Z3_ast, Value*>& valueMap);

	Value* translateExpr(z3::expr expr, SimpleSection::Builder builder,
		std::unordered_map<Z3_ast, Value*>& valueMap);

	Value* translateRelationExpr(z3::expr expr, SimpleSection::Builder builder,
		std::unordered_map<Z3_ast, Value*>& valueMap, const char* opName);

	std::vector<Value*> translateSubExpr(z3::expr expr, SimpleSection::Builder builder,
		std::unordered_map<Z3_ast, Value*>& valueMap);

	void createParameters(const std::unordered_map<Z3_ast, Value*>& srcValueMap, Section::Builder dstSectionBuilder,
		SectionCall::Builder& dstCallBuilder, std::unordered_map<Z3_ast, Value*>& dstValueMap);

	OperationValue* translateTupleGet(z3::expr expr, SimpleSection::Builder builder,
		std::unordered_map<Z3_ast, Value*>& valueMap);

public:
	std::vector<Summary> summaryVector;
	z3::context& z3ctx;
	std::unordered_map<Z3_ast, Value*> outerValueMap;
	std::unordered_map<Z3_ast, std::pair<Z3_ast, size_t>> tupleElemMap;

private:
	std::unordered_map<PhiValue*, SummaryBuilders> summaryBuildersMap;
};
