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
		std::cout << "index:\n" << write.index << std::endl;
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
#ifdef _DEBUG
		std::cout << "existCondEquation:\n" << write.existCond << std::endl;
#endif // _DEBUG
		auto ec = simplifyUseTactic(write.writeCond.substitute(v_x, dst_sk) && write.skolemCond);
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
