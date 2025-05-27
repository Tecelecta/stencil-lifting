#pragma once

#include "PassBase.h"
#include "TypeToZ3Visitor.h"
#include "ConstantToZ3Visitor.h"
#include "Summary.h"
#include "ContainerHash.h"

class SectionToZ3Pass : virtual public Pass<GraphCallPath>,
	protected TrivialSccAction, protected Value::Visitor<bool, uint32_t, const std::vector<SectionCall*>&>
{
public:
	explicit SectionToZ3Pass(z3::context& z3ctx);

	bool run();

	bool update(uint32_t i);

	bool update(const vertex_list& initUpdateList);

	void invalidate(const vertex_list& arrayBaseList);

	z3::expr getLoopBoundCond(const z3::expr& bound, const z3::expr& begin, const z3::expr& times, const z3::expr& step) const;

protected:
	bool runOnTrivial(uint32_t i) override;

	bool expandSection(Section* section) const override
	{
		return dynamic_cast<IterateLoop*>(section) == nullptr;
	}

private:
	z3::func_decl getUninterprered(Operation op);

public:
	struct TupleInfo
	{
		z3::expr elem;
		z3::expr base;
		size_t index;
	};

	z3::context& z3ctx;
	std::vector<Summary> summaryVector;
	std::unordered_map<uint32_t, z3::expr> parallelLoopBoundMap;
	std::unordered_map<uint32_t, z3::expr> invalidValueMap;
	std::unordered_map<Z3_ast, std::pair<Z3_ast, size_t>> tupleElemMap;
	std::unordered_map<std::pair<Z3_ast, size_t>, uint32_t> tupleValueMap;
	TypeToZ3Visitor typeVisitor;
	ConstantToZ3Visitor constantVisitor;
	std::unordered_map<std::string_view, SummaryFunction> opMap;
	std::unordered_map<std::string_view, FormFunction> formMap;
	std::unordered_map<Operation, z3::func_decl> z3FuncMap;
};
