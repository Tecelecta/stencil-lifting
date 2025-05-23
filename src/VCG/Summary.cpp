﻿#include "Summary.h"

#include <sstream>

z3::expr_vector Summary::Branch::createIndexVector() const
{
	z3::expr_vector result(elem.ctx());
	for (const auto& a : index)
	{
		result.push_back(a.func);
	}
	return result;
}

Summary Summary::createRange(z3::expr value)
{
	return Summary(Summary::Form::RANGE, value);
}

Summary Summary::createRange(z3::context& z3ctx, uint32_t i)
{
	std::stringstream ss;
	ss << "s_" << i;
	return createRange(z3ctx.bool_const(ss.str().c_str()));
}

Summary Summary::createScalar(z3::expr elem, Form form)
{
	Summary result(form, elem.ctx().constant("invalid", elem.get_sort()));
	result.addBranch({ elem.ctx().bool_val(true), std::move(elem) });
	return result;
}

Summary Summary::createScalar(z3::expr cond, z3::expr elem, Form form)
{
	Summary result(form, elem.ctx().constant("invalid", elem.get_sort()));
	result.addBranch({ std::move(cond), std::move(elem) });
	return result;
}

Summary Summary::initScalar(z3::sort z3_sort, uint32_t i, Form form)
{
	std::stringstream ss;
	ss << "s_" << i;
	Summary result(z3_sort, form);
	result.addBranch({ z3_sort.ctx().bool_val(true), z3_sort.ctx().constant(ss.str().c_str(), z3_sort)});
	return result;
}

Summary Summary::initArrayBase(z3::sort z3_sort, uint32_t i)
{
	std::stringstream ss;
	ss << "a_" << i;
	Summary result(z3_sort, Summary::Form::ARRAY);
	result.base = z3_sort.ctx().constant(ss.str().c_str(), z3_sort);
	return result;
}

Summary Summary::copyBase() const
{
	return Summary(form, base);
}

z3::expr Summary::iteStoreForm() const
{
	switch (form)
	{
	case Summary::Form::ELEM:
	case Summary::Form::INDEX:
	case Summary::Form::RANGE: {
		z3::expr result = base;
		for (const auto& item : branches)
		{
			assert(item.index.empty());
			if (item.cond.is_true())
			{
				result = item.elem;
			}
			else
			{
				result = z3::ite(item.cond, item.elem, result);
			}
		}
		return result;
	}
	case Summary::Form::ARRAY: {
		z3::expr result = base;
		for (const auto& item : branches)
		{
			assert(!item.index.empty());
			if (item.cond.is_true())
			{
				result = z3::store(result, item.createIndexVector(), item.elem);
			}
			else
			{
				result = z3::ite(item.cond, z3::store(result, item.createIndexVector(), item.elem), result);
			}
		}
		return result;
	}
	default:
		assert(false);
		return z3::expr(base.ctx());
	}
}

std::string Summary::str() const
{
	std::stringstream ss;
	switch (form)
	{
	case Summary::Form::ELEM:
		ss << "ELEM:\n";
		for (const auto& item : branches)
		{
			assert(item.index.empty());
			ss << "  cond:\n  " << item.cond << "\n  value:\n  " << item.elem << '\n';
		}
		break;
	case Summary::Form::INDEX:
		ss << "INDEX:\n";
		for (const auto& item : branches)
		{
			assert(item.index.empty());
			ss << "  cond:\n  " << item.cond << "\n  value:\n  " << item.elem << '\n';
		}
		break;
	case Summary::Form::RANGE:
		ss << "RANGE:\n  " << base << '\n';
		break;
	case Summary::Form::ARRAY:
		ss << "ARRAY:\n";
		if (branches.empty())
		{
			ss << "  " << base << '\n';
		}
		for (const auto& item : branches)
		{
			assert(!item.index.empty());
			ss << "  cond:\n  " << item.cond << "\n  layer:\n  " << item.layer << "\n  value:\n  ";
			ss << z3::store(base, item.createIndexVector(), item.elem) << '\n';
		}
		break;
	default:
		ss << "ERROR!!!";
		break;
	}
	return ss.str();
}

void Summary::addBranch(Branch src_branch)
{
	branches.emplace_back(std::move(src_branch));
}

bool Summary::canMerge(const Summary::Branch& a, const Summary::Branch& b)
{
	switch (form)
	{
	case Summary::Form::ELEM:
		return (Z3_ast)a.elem == (Z3_ast)b.elem;
	case Summary::Form::INDEX:
		return proveEquals(a.elem, b.elem);
	case Summary::Form::ARRAY: {
		if (a.index.size() != b.index.size())
		{
			return false;
		}
		for (uint32_t i = 0; i < a.index.size(); i++)
		{
			if ((Z3_ast)a.index[i].func != (Z3_ast)b.index[i].func)
			{
				return false;
			}
		}
		return (Z3_ast)a.elem == (Z3_ast)b.elem;
	}
	default:
		return false;
	}
}

bool Summary::mergeBranch(Branch src_branch)
{
	if (src_branch.cond.is_false())
	{
		return true;
	}
	for (auto& dst_branch : branches)
	{
		if (canMerge(dst_branch, src_branch))
		{
			dst_branch.cond = simplifyUseTactic(dst_branch.cond || src_branch.cond);
			return true;
		}
	}
	addBranch(std::move(src_branch));
	return false;
}

void Summary::mergeBranches(const std::vector<Branch>& src_branches)
{
	for (auto& src_branch : src_branches)
	{
		mergeBranch(src_branch);
	}
}

bool Summary::checkDistinct() const
{
	if (!branches.empty())
	{
		for (size_t i = 0; i < branches.size(); i++)
		{
			for (size_t j = 0; j < branches.size(); j++)
			{
				if (i != j && !proveFalse(branches[i].cond && branches[j].cond))
				{
					std::cerr << str() << std::endl;
					return false;
				}
			}
		}
	}
	return true;
}

static void createRangeSummary(Summary& result, const std::vector<Summary>& srcVector,
	z3::expr cond, const SummaryFunction& op, std::vector<z3::expr>& operands)
{
	if (operands.size() < srcVector.size())
	{
		const auto& src = srcVector[operands.size()];
		if (src.form == Summary::Form::RANGE)
		{
			operands.push_back(src.base);
			createRangeSummary(result, srcVector, cond, op, operands);
			operands.pop_back();
		}
		else
		{
			assert(src.form == Summary::Form::INDEX);
			for (const auto& branch : src.branches)
			{
				auto next_cond = simplifyUseTactic(cond && branch.cond);
				if (!next_cond.is_false())
				{
					operands.push_back(branch.elem);
					createRangeSummary(result, srcVector, next_cond, op, operands);
					operands.pop_back();
				}
			}
		}
	}
	else
	{
		result.base = z3::ite(cond, op(operands), result.base).simplify();
	}
}

Summary createRangeSummary(const std::vector<Summary>& srcVector, const SummaryFunction& op, z3::context& z3ctx)
{
	std::vector<z3::expr> operands;
	auto result = Summary::createRange(z3ctx.bool_val(false));
	createRangeSummary(result, srcVector, z3ctx.bool_val(true), op, operands);
	result.base = simplifyUseTactic(result.base);
	return result;
}

static void createScalarSummary(Summary& result, size_t layer, const std::vector<Summary>& srcVector,
	z3::expr cond, const SummaryFunction& op, std::vector<z3::expr>& operands)
{
	if (operands.size() < srcVector.size())
	{
		const auto& src = srcVector[operands.size()];
		if (src.form == Summary::Form::RANGE)
		{
			operands.push_back(src.base);
			createScalarSummary(result, layer, srcVector, cond, op, operands);
			operands.pop_back();
		}
		else
		{
			assert(src.form == Summary::Form::ELEM || src.form == Summary::Form::INDEX);
			for (const auto& branch : src.branches)
			{
				auto next_cond = simplifyUseTactic(cond && branch.cond);
				if (!next_cond.is_false())
				{
					operands.push_back(branch.elem);
					createScalarSummary(result, layer, srcVector, next_cond, op, operands);
					operands.pop_back();
				}
			}
		}
	}
	else
	{
		result.mergeBranch({ cond, op(operands) });
	}
}

Summary createScalarSummary(size_t layer, const std::vector<Summary>& srcVector, const SummaryFunction& op, z3::sort z3_sort, Summary::Form form)
{
	std::vector<z3::expr> operands;
	Summary result(z3_sort, form);
	createScalarSummary(result, layer, srcVector, z3_sort.ctx().bool_val(true), op, operands);
	assert(!result.branches.empty());
	return result;
}

static void createArrayGetSummary(Summary& result, size_t layer, const std::vector<Summary>& srcVector,
	const z3::expr_vector& arrayBound, z3::expr cond, std::vector<z3::expr>& readIndex)
{
	auto& z3ctx = arrayBound.ctx();
	auto n = srcVector.size() - 1;
	if (readIndex.size() < n)
	{
		// 第一阶段：根据下标的前置条件，排除不会运行到的分支
		const auto& src = srcVector[readIndex.size() + 1];
		assert(src.form == Summary::Form::INDEX);
		for (const auto& branch : src.branches)
		{
			auto next_cond = simplifyUseTactic(cond && branch.cond);
			if (!next_cond.is_false())
			{
				readIndex.push_back(branch.elem);
				createArrayGetSummary(result, layer, srcVector, arrayBound, next_cond, readIndex);
				readIndex.pop_back();
			}
		}
	}
	else
	{
		// 第二阶段：根据数组的每个写分支，确定读到的元素
		auto read_index = createZ3Vector(z3ctx, readIndex);
		const auto& arr = srcVector.front();
		assert(arr.form == Summary::Form::ARRAY);
		z3::expr no_write_cond = cond;
		for (const auto& branch : arr.branches)
		{
			// 处理写过的下标
			no_write_cond = no_write_cond && !branch.cond;
			auto read_cond = simplifyUseTactic((cond && branch.cond).substitute(arrayBound, read_index));
			if (!read_cond.is_false())
			{
				z3::expr read_elem = branch.elem;
				read_elem = read_elem.substitute(arrayBound, read_index);
				result.mergeBranch({ read_cond, read_elem });
			}
		}
		// 处理没写过的下标
		no_write_cond = simplifyUseTactic(no_write_cond.substitute(arrayBound, read_index));
		if (!no_write_cond.is_false())
		{
			result.mergeBranch({ no_write_cond, arr.base[read_index].simplify() });
		}
	}
}

Summary createArrayGetSummary(size_t layer, const std::vector<Summary>& srcVector, const z3::expr_vector& arrayBound, z3::sort z3_sort)
{
	std::vector<z3::expr> readIndex;
	Summary result(z3_sort, Summary::Form::ELEM);
	createArrayGetSummary(result, layer, srcVector, arrayBound, z3_sort.ctx().bool_val(true), readIndex);
	return result;
}

static void createArraySetSummary(Summary& result, size_t layer, const std::vector<Summary>& srcVector,
	const z3::expr_vector& arrayBound, z3::expr cond, std::vector<Summary::WriteIndex>& writeIndex)
{
	auto& z3ctx = arrayBound.ctx();
	auto n = srcVector.size() - 2;
	if (writeIndex.size() < n)
	{
		// 第一阶段：根据下标的前置条件，排除不会运行到的分支
		const auto& src = srcVector[writeIndex.size() + 1];
		assert(src.form == Summary::Form::INDEX);
		for (const auto& branch : src.branches)
		{
			auto next_cond = simplifyUseTactic(cond && branch.cond);
			if (!next_cond.is_false())
			{
				writeIndex.push_back({ branch.elem });
				createArraySetSummary(result, layer, srcVector, arrayBound, next_cond, writeIndex);
				writeIndex.pop_back();
			}
		}
	}
	else
	{
		// 第二阶段：根据被写元素的前置条件，确定被写的分支
		const auto& src = srcVector.back();
		assert(src.form != Summary::Form::ARRAY);
		for (const auto& branch : src.branches)
		{
			z3::expr write_cond = cond && branch.cond;
			for (uint32_t i = 0; i < n; i++)
			{
				write_cond = write_cond && arrayBound[i] == writeIndex[i].func;
			}
			write_cond = simplifyUseTactic(write_cond);
			if (!write_cond.is_false())
			{
				result.mergeBranch({ write_cond, branch.elem, layer, writeIndex });
			}
		}
	}
}

Summary createArraySetSummary(size_t layer, const std::vector<Summary>& srcVector, const z3::expr_vector& arrayBound)
{
	auto& z3ctx = arrayBound.ctx();
	std::vector<Summary::WriteIndex> writeIndex;
	auto result = srcVector.front().copyBase();
	createArraySetSummary(result, layer, srcVector, arrayBound, z3ctx.bool_val(true), writeIndex);

	// 第三阶段：将原数组的每个未被复写的分支添加进来
	z3::expr override_cond = z3ctx.bool_val(false);
	for (const auto& branch : result.branches)
	{
		override_cond = override_cond || branch.cond;
	}
	override_cond = override_cond.simplify();
	const auto& arr = srcVector.front();
	assert(arr.form == Summary::Form::ARRAY);
	for (const auto& branch : arr.branches)
	{
		auto remain_cond = simplifyUseTactic(!override_cond && branch.cond);
		if (!remain_cond.is_false())
		{
			result.mergeBranch({ remain_cond, branch.elem, branch.layer, branch.index, branch.skolem_x });
		}
	}
	return result;
}

std::vector<z3::expr> getArrayBound(size_t dims)
{
	return std::vector<z3::expr>();
}

z3::expr_vector getArrayBound(z3::context& z3ctx, size_t dims)
{
	z3::expr_vector w(z3ctx);
	for (size_t i = 0; i < dims; i++)
	{
		std::stringstream ss;
		ss << "w_" << i + 1;
		w.push_back(z3ctx.int_const(ss.str().c_str()));
	}
	return w;
}
