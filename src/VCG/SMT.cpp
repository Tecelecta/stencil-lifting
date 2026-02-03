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
	z3::expr& scale, z3::expr& offset, bool& isAffine, bool& isConstant)
{
	assert(src_x.size() == 1 && dst_0.size() == 1 && dst_1.size() == 1);
    auto x = src_x[0];
	if (isFunctionOf(y, x))
	{
		auto y0 = y.substitute(src_x, dst_0);
        auto y1 = y.substitute(src_x, dst_1);
		scale = (y1 - y0).simplify();
		offset = y0.simplify();

        auto f = (x * scale + offset).simplify();
        isAffine = scale.is_numeral() && proveEquals(f, y);
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

void sovleNonUnitAffine(z3::expr y, z3::expr_vector src_x, z3::expr& scale,
                        z3::expr& offset, bool& isAffine, bool& isConstant, int step)
{
    assert(src_x.size() == 1);
    auto& z3ctx = y.ctx();
    auto x = src_x[0];
    assert(isFunctionOf(y, x));
    for (int extra=0; extra<8; ++extra)
    {
        z3::expr_vector dst_0(z3ctx);
        z3::expr_vector dst_1(z3ctx);
        dst_0.push_back(z3ctx.int_val(extra));
        dst_1.push_back(z3ctx.int_val(extra+step));
        
        auto y0 = y.substitute(src_x, dst_0);
        auto y1 = y.substitute(src_x, dst_1);
        scale = ((y1 - y0)/step).simplify();
        offset = (y0 - extra*scale).simplify();
        // handling non-unit step affine function
        auto f = ((step+extra+(x-step-extra)/step*step) * scale + offset).simplify();
        isAffine = scale.is_numeral() && proveEquals(f, y);
        if (isAffine) break;
    }
    isConstant = false;
}

bool match(const z3::expr& expr, Pattern p, z3::expr_vector& res)
{
    // match root node
    if (expr.decl().decl_kind() == p.kind)
    {
        res.push_back(expr);
        // match children nodes
        if (p.child.has_value())
        {
            bool found = false;
            for(auto& child : p.child.value())
            {
                found = false;
                for (const auto& arg : expr.args())
                {
                    if ( (found = match(arg, child, res)) )
                    {
                        break;
                    }
                }
                if (!found) break;
            }
            return found;
        }
        return true;
    }
    return false;
}

z3::expr_vector search(const z3::expr& expr, Pattern p)
{
    z3::expr_vector res(expr.ctx());
    if (match(expr, p, res))
    {
        return res;
    }

    for (const auto& arg : expr.args())
    {
        auto ret = search(arg, p);
        if (!ret.empty()) return ret;
    }
    return z3::expr_vector(expr.ctx());
}

z3::expr_vector searchAll(const z3::expr& expr, Pattern p)
{
    z3::expr_vector match_res(expr.ctx());
    if (match(expr, p, match_res))
    {
        return match_res;
    }
    
    z3::expr_vector search_res(expr.ctx());
    for (const auto& arg : expr.args())
    {
        auto child = searchAll(arg, p);
        if (!child.empty())
        {
            for (auto m: child)
                search_res.push_back(m);
        }
    }
    return search_res;
}

int searchNonUnitStep(const z3::expr& expr)
{
    Pattern mul_div_val{
        Z3_OP_MUL, {
            {Z3_OP_IDIV, {
                {Z3_OP_ANUM}
            }},
            {Z3_OP_ANUM}
        }
    };
    
    auto res = search(expr, mul_div_val);
    if (!res.empty())
    {
        if (res[2].is_int() && res[3].is_int() && proveTrue(res[2] == res[3]))
        {
            return res[2].get_numeral_int();
        }
    }
    assert(false);
    return -1;
}

void pealMulDivConst(const z3::expr& expr, z3::expr_vector& src, z3::expr_vector& dst)
{
    Pattern mul_div_val{
        Z3_OP_MUL, {
            {Z3_OP_IDIV, {
                {Z3_OP_ANUM}
            }},
            {Z3_OP_ANUM}
        }
    };
    
    auto res = searchAll(expr, mul_div_val);
    if (!res.empty())
    {
        int cursor = 0;
        while (cursor < res.size())
        {
            if (res[cursor+2].is_int() && res[cursor+3].is_int()
                && proveTrue(abs(res[cursor+2]) == abs(res[cursor+3])))
            {
                src.push_back(res[cursor]);
                for (const auto& arg : res[cursor+1].args())
                {
                    if ( (Z3_ast)res[cursor+2] != (Z3_ast)arg )
                    {
                        dst.push_back(arg);
                    }
                }
            }
            cursor += 4;
        }
    }
    else assert(false);
}
