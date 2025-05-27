#pragma once

#include "ArrayAccessVisitor.h"
#include "Summary.h"

struct ArrayWriteCond
{
	ArrayWriteCond(z3::context& z3ctx, Summary::Branch branch)
		: index(z3ctx), writeCond(z3ctx), existCond(z3ctx), validCond(z3ctx),
		skolemFunc(z3ctx), skolemCond(z3ctx), branch(std::move(branch)) {}

	Summary::Branch branch;
	z3::expr_vector index;
	z3::expr writeCond;
	z3::expr existCond;
	z3::expr validCond;
	z3::expr skolemFunc;
	z3::expr skolemCond;
	bool hasAffine = false;
	bool hasSkolem = false;
	bool allConstant = true;
};

class ArraySetSolver
{
public:
	ArraySetSolver(z3::context& z3ctx, z3::expr times, size_t numDims, Summary summary);

	bool solve();

private:
	void initWriteCond();

	void solveAffineAndSkolem(ArrayWriteCond& write);

	bool solveExistCond(ArrayWriteCond& write);

	bool solveValidCond(ArrayWriteCond& write);

public:
	z3::context& z3ctx;
	z3::expr_vector w;
	z3::expr t;
	z3::expr x;
	z3::expr y;
	z3::expr t0;
	z3::expr times;
	z3::expr roi;
	Summary summary;
	std::vector<ArrayWriteCond> writeConds;

private:
	z3::expr_vector v_t;
	z3::expr_vector v_x;
	z3::expr_vector v_y;
	z3::expr_vector src_t0;
	z3::expr_vector dst_0;
	z3::expr_vector dst_1;
	z3::expr_vector dst_times;
};
