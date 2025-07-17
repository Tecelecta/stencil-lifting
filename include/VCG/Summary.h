#pragma once

#include "SMT.h"

#include <optional>
#include <iostream>

class Summary final
{
public:
	enum class Form
	{
		NONE, ELEM, INDEX, RANGE, ARRAY
	} form = Form::NONE;
	z3::expr base;

	explicit Summary(z3::context& z3ctx) : base(z3ctx) {}

	Summary(z3::sort z3_sort, Summary::Form form) : base(z3_sort.ctx().constant("invalid", z3_sort)), form(form) {}

private:
	Summary(Summary::Form form, z3::expr base) : form(form), base(std::move(base)) {}

public:
	struct WriteIndex
	{
		z3::expr func;
		std::optional<z3::expr> bound;
		std::optional<z3::expr> skolem_b;
        std::optional<z3::expr> restore_w;
	};

	struct Branch
	{
		z3::expr_vector createIndexVector() const;

		z3::expr cond;
		z3::expr elem;
		size_t layer = 0;
		std::vector<WriteIndex> index;
		std::optional<z3::expr> skolem_x;
	};

	static Summary createRange(z3::expr value);

	static Summary createRange(z3::context& z3ctx, uint32_t i);

	static Summary createScalar(z3::expr elem, Form form);

	static Summary createScalar(z3::expr cond, z3::expr elem, Form form);

	static Summary initScalar(z3::sort z3_sort, uint32_t i, Form form);

	static Summary initArrayBase(z3::sort z3_sort, uint32_t i);

	Summary copyBase() const;

	z3::expr iteStoreForm() const;

	std::string str() const;

	void addBranch(Branch src_branch);

	bool canMerge(const Summary::Branch& a, const Summary::Branch& b);

	bool mergeBranch(Branch src_branch);

	void mergeBranches(const std::vector<Branch>& src_branches);

	bool checkDistinct() const;

public:
	std::vector<Branch> branches;
};

typedef std::function<z3::expr(const std::vector<z3::expr>&)> SummaryFunction;

typedef std::function<Summary::Form(const std::vector<Summary::Form>&)> FormFunction;

Summary createRangeSummary(const std::vector<Summary>& srcVector, const SummaryFunction& op, z3::context& z3ctx);

Summary createScalarSummary(size_t layer, const std::vector<Summary>& srcVector, const SummaryFunction& op, z3::sort z3_sort, Summary::Form form);

Summary createArrayGetSummary(size_t layer, const std::vector<Summary>& srcVector, const z3::expr_vector& arrayBound, z3::sort z3_sort);

Summary createArraySetSummary(size_t layer, const std::vector<Summary>& srcVector, const z3::expr_vector& arrayBound);

z3::expr_vector getArrayBound(z3::context& z3ctx, size_t dims);

static inline bool has(const z3::expr& expr, const std::string& key)
{
    return expr.to_string().find(key) != std::string::npos;
}

static inline z3::expr find_const_subexpr(const z3::expr& expr, const std::string& key)
{
    if (expr.is_const())
    {
        if ( has(expr, key) )
        {
            return expr;
        }
        else
        {
            return expr.ctx().bool_val(false);
        }
    }
    else
    {
        auto ret = find_const_subexpr(expr.arg(0), key);
        for (int i=1; i<expr.num_args(); i++)
        {
            if (!ret.is_false())
            {
                return ret;
            }
            ret = find_const_subexpr(expr.arg(i), key);
        }
        return ret;
    }
}
