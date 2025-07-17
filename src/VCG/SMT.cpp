#include "SMT.h"

bool proveTrue(z3::expr a)
{
	a = a.simplify();
	if (a.is_true())
	{
		return true;
	}
	if (a.is_false())
	{
		return false;
	}
	z3::solver solver(a.ctx());
	solver.add(!a);
	return solver.check() == z3::unsat;
}

bool proveFalse(z3::expr a)
{
	a = a.simplify();
	if (a.is_false())
	{
		return true;
	}
	if (a.is_true())
	{
		return false;
	}
	z3::solver solver(a.ctx());
	solver.add(a);
	return solver.check() == z3::unsat;
}

bool proveZero(z3::expr a)
{
	return proveTrue(a == 0);
}

bool proveNotZero(z3::expr a)
{
	return proveFalse(a == 0);
}

bool proveEquals(z3::expr a, z3::expr b)
{
	return (Z3_ast)a == (Z3_ast)b || proveTrue(a == b);
}

bool proveNotEquals(z3::expr a, z3::expr b)
{
	return (Z3_ast)a != (Z3_ast)b && proveFalse(a == b);
}

bool isFunctionOf(z3::expr parent, z3::expr child)
{
	auto& z3ctx = parent.ctx();
	z3::expr_vector src(z3ctx);
	src.push_back(child);
	z3::expr_vector dst(z3ctx);
	dst.push_back(z3ctx.constant(nullptr, child.get_sort()));
	return !proveEquals(parent, parent.substitute(src, dst));
}

z3::expr_vector createZ3Vector(z3::context& z3ctx, const std::vector<z3::expr>& vec)
{
	z3::expr_vector result(z3ctx);
	for (const auto& a : vec)
	{
		result.push_back(a);
	}
	return result;
}

z3::expr simplifyUseTactic(z3::expr src, bool elim_and)
{
	z3::tactic t(src.ctx(), "simplify");
	t = elim_and ? t & z3::tactic(src.ctx(), "elim-and") : t;
	t = t & z3::tactic(src.ctx(), "symmetry-reduce");
	t = t & z3::tactic(src.ctx(), "propagate-ineqs");
	t = t & z3::tactic(src.ctx(), "propagate-values2");
	t = t & z3::tactic(src.ctx(), "ctx-solver-simplify");
	z3::goal g(src.ctx());
	g.add(src);
	return t(g)[0].as_expr();
}

void solveAffine(z3::expr y, z3::expr_vector src_x, z3::expr_vector dst_0, z3::expr_vector dst_1,
	z3::expr& scale, z3::expr& offset, bool& isAffine, bool& isConstant, int step)
{
	assert(src_x.size() == 1 && dst_0.size() == 1 && dst_1.size() == 1);
    dst_1[0] = dst_1[0];
    auto x = src_x[0];
	if (isFunctionOf(y, x))
	{
		auto y0 = y.substitute(src_x, dst_0);
		auto y1 = y.substitute(src_x, dst_1);
		scale = (y1 - y0).simplify();
		offset = y0.simplify();
		auto f = (scale * x + offset).simplify();
		isAffine = scale.is_numeral() && proveEquals(f, y); // TODO: 变长缩放因子
		isConstant = false;
	}
	else
	{
		scale = dst_0[0];
		offset = std::move(y);
		isAffine = true;
		isConstant = true;
	}
}

static void _find_nonunit_step(z3::expr expr, z3::expr& outer, z3::expr& inner, z3::expr& scale)
{
    if (expr.is_const())
    {
        return;
    }
    else
    {
        auto kind = expr.decl().decl_kind();
        if (kind == Z3_OP_IDIV)
        {
        }
    }
}

z3::expr looseNonUnitStep(const z3::expr& expr)
{
    
}
