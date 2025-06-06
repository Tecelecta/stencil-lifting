#include "ArraySetSolver.h"

ArraySetSolver::ArraySetSolver(z3::context& z3ctx, z3::expr times, size_t numDims, Summary summary)
	: z3ctx(z3ctx), w(getArrayBound(z3ctx, numDims)),
	t(z3ctx.int_const("t")), x(z3ctx.int_const("x")), y(z3ctx.int_const("y")),
	t0(z3ctx.int_const("t0")), times(times), roi(z3ctx), summary(std::move(summary)),
	v_t(z3ctx), src_t0(z3ctx), dst_0(z3ctx), dst_1(z3ctx), dst_times(z3ctx), v_x(z3ctx), v_y(z3ctx)
{
	v_t.push_back(t);
	v_x.push_back(x);
	v_y.push_back(y);
	src_t0.push_back(t0);
	dst_0.push_back(z3ctx.int_val(0));
	dst_1.push_back(z3ctx.int_val(1));
	dst_times.push_back(times);
}

static bool has(const z3::expr& expr, const std::string& key)
{
	return expr.to_string().find(key) != std::string::npos;
}

static z3::expr find_w_bound(z3::expr expr, z3::expr key, bool is_ub, bool dft = false)
{
	auto kind = expr.decl().decl_kind();
	auto ret = expr.ctx().bool_val(dft);
	auto skey = key.to_string();
	if (kind == Z3_OP_OR)
	{
		for (auto const arg : expr.args())
		{
			if (has(arg, skey))
				ret = ret || find_w_bound(arg, key, is_ub);
		}
	}
	else if (kind == Z3_OP_AND)
	{		
		ret = expr.ctx().bool_val(true);
		for (auto const arg : expr.args())
		{
			if (has(arg, skey))
				ret = ret && find_w_bound(arg, key, is_ub, true);
		}
	}
	else if (kind == Z3_OP_ITE)
	{
		if (has(expr.arg(1), skey)) 
		{
			ret = find_w_bound(expr.arg(0) && expr.arg(1), key, is_ub);
		}
		if (has(expr.arg(2), skey))
		{
			ret = ret || find_w_bound((!expr.arg(0)) && expr.arg(2), key, is_ub);
		}
	}
	else if (kind == Z3_OP_NOT)
	{
		expr = expr.arg(0);
		auto lk = expr.decl().decl_kind();
		assert(lk == Z3_OP_GE || lk == Z3_OP_LE || kind == Z3_OP_EQ);
		is_ub = !is_ub;
		if (has(expr, skey))
		{
			if (has(expr.arg(0), skey) && 
				( kind == Z3_OP_GE && !is_ub || kind == Z3_OP_LE && is_ub))
			{
				assert(!has(expr.arg(1), skey));
				ret = !expr;
			}
			else if (has(expr.arg(1), skey) &&
				( kind == Z3_OP_GE && is_ub || kind == Z3_OP_LE && !is_ub))
			{
				assert(!has(expr.arg(0), skey));
				ret = !expr;
			}
			else if (kind == Z3_OP_EQ)
			{
				ret = !expr;
			}
		}
	}
	else
	{
		assert(kind == Z3_OP_GE || kind == Z3_OP_LE || kind == Z3_OP_EQ);
		auto skey = key.to_string();
		if (has(expr, skey))
		{
			if (has(expr.arg(0), skey) && 
				( kind == Z3_OP_GE && !is_ub || kind == Z3_OP_LE && is_ub))
			{
				assert(!has(expr.arg(1), skey));
				ret = expr;
			}
			else if (has(expr.arg(1), skey) &&
				( kind == Z3_OP_GE && is_ub || kind == Z3_OP_LE && !is_ub))
			{
				assert(!has(expr.arg(0), skey));
				ret = expr;
			}
			else if (kind == Z3_OP_EQ)
			{
				ret = expr;
			}
		}
	}
	return simplifyUseTactic(ret);
}

static z3::expr __rcw(z3::expr src)
{
	auto kind = src.decl().decl_kind();
	auto ret = src.ctx().bool_val(false);
	switch (kind)
	{
	case Z3_OP_AND:
	{
		ret = src.ctx().bool_val(true);
		for (const auto& arg : src.args())
		{
			auto lk = arg.decl().decl_kind();
			assert(lk == Z3_OP_GE || lk == Z3_OP_LE || lk == Z3_OP_EQ || lk == Z3_OP_ITE);
			if (!has(arg, "__remove__"))
			{
				ret = ret && arg;
			}
		}
		return ret;
	}
	case Z3_OP_ITE:
	{
		assert(src.get_sort().is_bool());
		if (!has(src.arg(0), "__remove__"))
		{
			if (!has(src.arg(1), "__remove__"))
			{
				ret = __rcw(src.arg(0) && src.arg(1));
			}
			if (!has(src.arg(2), "__remove__"))
			{
				ret = ret || __rcw((!src.arg(0)) && src.arg(2));
			}
		}
		return ret;
	}
	case Z3_OP_OR:
	{
		for (const auto& arg : src.args())
		{
			auto lk = arg.decl().decl_kind();
			assert(lk == Z3_OP_GE || lk == Z3_OP_LE || lk == Z3_OP_EQ || lk == Z3_OP_ITE);
			if (!has(arg, "__remove__"))
			{
				ret = ret || arg;
			}
		}
		return ret;
	}
	default:
		assert(false);
	}

}

static z3::expr remove_comparison_with(z3::expr src, const z3::expr pat)
{
	auto& ctx = src.ctx();
	z3::expr_vector pre_v(ctx);
	z3::expr_vector post_v(ctx);
	pre_v.push_back(pat);
	post_v.push_back(ctx.int_const("__remove__"));
	auto tmp = src.substitute(pre_v, post_v);
	return __rcw(tmp);
}

bool ArraySetSolver::tryBypassTiling(const z3::expr it_times)
{
	bool bypass = true;
	int tiled_dim=-1;
	for (const auto& br : summary.branches)
	{
		// confirm all index are independent from t
		for (int i = 0; i < br.index.size(); ++i)
		{
			auto subs = br.index[i].func;
			bool indep = !has(br.index[i].func, "t");
			if (indep && !has(br.index[i].func, "s_"))
			{
				tiled_dim = i;
			}
			bypass &= indep;
		}
	}
	if (bypass)
	{
		// replace `t` with expr of `tm-1` and `0` in cond
		for (auto& br : summary.branches)
		{
			z3::expr_vector tile_dst(z3ctx);
			// tile_dst.push_back((it_times - 1).simplify());
			
			// auto lb_cond = find_w_bound(br.cond, w[tiled_dim], false).substitute(v_t, dst_0);
			// auto ub_cond = find_w_bound(br.cond, w[tiled_dim], true).substitute(v_t, tile_dst);
			// br.cond = simplifyUseTactic(remove_comparison_with(br.cond, t) && ub_cond && lb_cond);

			z3::expr_vector tile_src(z3ctx);
			std::stringstream ss;
			ss << "b_" << tiled_dim+1;
			tile_src.push_back(w[tiled_dim]);
			tile_dst.push_back(z3ctx.int_const(ss.str().c_str()));
			br.cond = simplifyUseTactic(br.cond.substitute(tile_src, tile_dst));
		}
	}
	return bypass;
}

bool ArraySetSolver::solve()
{
	initWriteCond();
	roi = z3ctx.bool_val(false);
	for (auto& write : writeConds)
	{
		solveExistCond(write);
		z3::expr_vector src_sk(z3ctx);
		z3::expr_vector dst_sk(z3ctx);
		for (const auto& index : write.branch.index)
		{
			if (index.bound.has_value() && index.skolem_b.has_value())
			{
				src_sk.push_back(index.bound.value());
				dst_sk.push_back(index.skolem_b.value());
			}
		}
		write.existCond = simplifyUseTactic(write.existCond.substitute(src_sk, dst_sk));
#ifdef _DEBUG
		std::cout << "existCond:\n" << write.existCond << std::endl;
#endif // _DEBUG
		roi = roi || write.existCond;
	}
	roi = simplifyUseTactic(roi);
#ifdef _DEBUG
	std::cout << "roi:\n" << roi << std::endl;
#endif // _DEBUG

	for (auto& write : writeConds)
	{
		solveValidCond(write);
		z3::expr_vector src_sk(z3ctx);
		z3::expr_vector dst_sk(z3ctx);
		for (const auto& index : write.branch.index)
		{
			if (index.bound.has_value() && index.skolem_b.has_value())
			{
				src_sk.push_back(index.bound.value());
				dst_sk.push_back(index.skolem_b.value());
			}
		}
		write.validCond = simplifyUseTactic(write.validCond.substitute(src_sk, dst_sk), true);
#ifdef _DEBUG
		std::cout << "validCond:\n" << write.validCond << std::endl;
#endif // _DEBUG
	}

	for (size_t i = 0; i < summary.branches.size(); i++)
	{
		auto& branch = summary.branches[i];	
		branch.cond = simplifyUseTactic(x == writeConds[i].skolemFunc && writeConds[i].validCond);
#ifdef _DEBUG
		std::cout << "branch " << i << " update:\n" << branch.cond << std::endl;
#endif // _DEBUG
		if (writeConds[i].hasSkolem)
		{
			branch.skolem_x = writeConds[i].skolemFunc;
		}
	}
	return true;
}

void ArraySetSolver::initWriteCond()
{
	assert(summary.form == Summary::Form::ARRAY);
	for (auto& branch : summary.branches)
	{
		auto& write = writeConds.emplace_back(z3ctx, branch);
		branch.cond = branch.cond.substitute(v_t, v_x);
		branch.elem = branch.elem.substitute(v_t, v_x);
		for (uint32_t j = 0; j < branch.index.size(); j++)
		{
			branch.index[j].func = branch.index[j].func.substitute(v_t, v_x);
		}
		write.writeCond = (0 <= t0 && t0 <= x && x <= t && t <= times - 1 && branch.cond).simplify();
		write.index = branch.createIndexVector();
#ifdef _DEBUG
		std::cout << "WriteIndex:\n" << write.index << std::endl;
		std::cout << "writeCond:\n"<< write.writeCond << std::endl;
#endif // _DEBUG
	}
}

void ArraySetSolver::solveAffineAndSkolem(ArrayWriteCond& write)
{
	for (uint32_t i = 0; i < write.index.size(); i++)
	{
		z3::expr scale(z3ctx);
		z3::expr offset(z3ctx);
		bool isAffine, isConstant;
		solveAffine(write.index[i], v_x, dst_0, dst_1, scale, offset, isAffine, isConstant);
		if (isAffine)
		{
			write.hasAffine = true;
			if (!isConstant)
			{
				write.allConstant = false;
				if (!write.hasSkolem)
				{
					write.hasSkolem = true;
					write.skolemFunc = ((w[i] - offset) / scale).simplify();
					write.skolemCond = (scale != 0 && (w[i] % scale == offset % scale)).simplify();
#ifdef _DEBUG
					std::cout << "skolemFunc:\n" << write.skolemFunc << std::endl;
					std::cout << "skolemCond:\n" << write.skolemCond << std::endl;
#endif // _DEBUG
				}
			}
		}
	}
	if (write.allConstant)
	{
		write.hasSkolem = true;
		write.skolemFunc = t;
		write.skolemCond = z3ctx.bool_val(true);
	}
}

bool ArraySetSolver::solveExistCond(ArrayWriteCond& write)
{
	solveAffineAndSkolem(write);
	if (write.hasSkolem)
	{
		z3::expr_vector dst_sk(z3ctx);
		dst_sk.push_back(write.skolemFunc);
		z3::expr_vector xs(z3ctx);
		for (const auto& index : write.branch.index)
		{
			if (index.bound.has_value() && index.skolem_b.has_value())
			{
				xs.push_back(index.bound.value());
			}
		}
		auto exist_write = write.writeCond;
		exist_write = exist_write.substitute(v_x, v_y);
		write.existCond = z3::exists(y, exist_write.simplify());
		auto ec = simplifyUseTactic(write.writeCond.substitute(v_x, dst_sk) && write.skolemCond);
#ifdef _DEBUG
		std::cout << "existCondEquation:\n" << write.existCond << std::endl;
		std::cout << "ec:\n" << ec << std::endl;
		std::cout << "xs(" << xs.size() << ")\n";
		for (const auto& x : xs) {
			std::cout << x << std::endl;
		}
		std::cout << std::endl;
#endif // _DEBUG
		assert(proveEquals(write.existCond, xs.empty() ? ec : z3::exists(xs, ec)));
		write.existCond = std::move(ec);
#ifdef _DEBUG
		std::cout << "existCondEquation (after):\n" << write.existCond << std::endl;
#endif // _DEBUG
		return true;
	}
	return false;
}

bool ArraySetSolver::solveValidCond(ArrayWriteCond& write)
{
	if (write.hasSkolem)
	{
		z3::expr_vector dst_sk(z3ctx);
		dst_sk.push_back(write.skolemFunc);
		z3::expr_vector dst_sk1(z3ctx);
		dst_sk1.push_back(write.skolemFunc + 1);
		z3::expr_vector dst_x1(z3ctx);
		dst_x1.push_back(x + 1);
		z3::expr_vector xs(z3ctx);
		for (const auto& index : write.branch.index)
		{
			if (index.bound.has_value() && index.skolem_b.has_value())
			{
				xs.push_back(index.bound.value());
			}
		}
		auto write0 = write.writeCond.substitute(src_t0, dst_0).simplify();
		auto valid_write = write0 && x <= t && !roi.substitute(src_t0, dst_x1);
		write.validCond = z3::exists(x, valid_write.simplify());
#ifdef _DEBUG
		std::cout << "validCondEquation:\n" << write.validCond << std::endl;
#endif // _DEBUG
		auto vc = simplifyUseTactic(write0.substitute(v_x, dst_sk) && !roi.substitute(src_t0, dst_sk1) && write.skolemCond);
		assert(proveEquals(write.validCond, xs.empty() ? vc : z3::exists(xs, vc)));
		write.validCond = std::move(vc);
		return true;
	}
	return false;
}
