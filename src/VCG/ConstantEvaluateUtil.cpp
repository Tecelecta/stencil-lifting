#include "ConstantEvaluateUtil.h"
#include "Context.h"

#include <cmath>

Value* ConstantEvaluateUtil::tryEvaluate(Context* context, Operation op, String name, const std::vector<ConstantValue*>& src)
{
	auto func_iter = evaluate.find(op.getName().view());
	if (func_iter == evaluate.end())
	{
		return nullptr;
	}
	auto result = (func_iter->second)(op, src, name);
	if (result == nullptr)
	{
		return context->createInvalidValue(op.getReturnType(), name);
	}
	return result;
}

size_t ConstantEvaluateUtil::getSelectedOperand(ConstantValue* selector)
{
	if (selector->getType().getName().equals("Logic"))
	{
		return selector->getValue<Logic>().isTrue() ? 1 : 2;
	}
	assert(false); // 使用整型选择未支持
	if (selector->getType().getName().equals("Integer"))
	{
		return selector->getValue<Integer>().getData().convert_to<size_t>() + 1;
	}
	assert(false);
	return 0;
}

template<typename SymbolType, typename UnaryOp>
inline SymbolType computeUnaryOp(const std::vector<ConstantValue*>& src, UnaryOp op = UnaryOp())
{
	assert(src.size() == 1);
	auto value = src[0]->getValue<SymbolType>();
	return op(value);
}

template<typename SymbolType, typename BinaryOp>
inline SymbolType computeBinaryOp(const std::vector<ConstantValue*>& src, BinaryOp op = BinaryOp())
{
	assert(src.size() == 2);
	auto left = src[0]->getValue<SymbolType>();
	auto right = src[1]->getValue<SymbolType>();
	return op(left, right);
}

template<typename SymbolType, typename BinaryOp>
inline SymbolType computeReduceOp(const std::vector<ConstantValue*>& src, BinaryOp op = BinaryOp())
{
	assert(!src.empty());
	return std::transform_reduce(src.begin() + 1, src.end(), src.front()->getValue<SymbolType>(), op,
		[](ConstantValue* value) -> SymbolType { return value->getValue<SymbolType>(); });
}

template<typename SymbolType, typename BinaryOp>
inline bool computeRelationOp(const std::vector<ConstantValue*>& src)
{
	assert(src.size() == 2);
	auto left = src[0]->getValue<SymbolType>();
	auto right = src[1]->getValue<SymbolType>();
	return BinaryOp()(left, right);
}

ConstantEvaluateUtil::ConstantEvaluateUtil()
{
	evaluate["copy"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			return src[0];
		};

	evaluate["select"] = [](Operation op, const std::vector<ConstantValue*>& src, String name) -> ConstantValue*
		{
			// 注：select算子需要特殊处理，只要选择子是常量即可
			// 此函数为了兼容已知操作数是常量的情况
			return src[getSelectedOperand(src[0])];
		};

	evaluate["bitcast"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			return op.getContext()->createConstantValue(op.getReturnType(), src[0]->getValue(), name);
		};

	evaluate["Integer.neg"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeUnaryOp<Integer, std::negate<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.add"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<Integer, std::plus<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.sub"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeBinaryOp<Integer, std::minus<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.mul"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<Integer, std::multiplies<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.div"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto type = op.getReturnType();
			auto left = src[0]->getValue<Integer>();
			auto right = src[1]->getValue<Integer>();
			if (right.isZero())
			{
				return static_cast<ConstantValue*>(nullptr);
			}
			return op.getContext()->createConstantValue(type, left / right, name);
		};

	evaluate["Integer.mod"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto type = op.getReturnType();
			auto left = src[0]->getValue<Integer>();
			auto right = src[1]->getValue<Integer>();
			if (right.isZero())
			{
				return static_cast<ConstantValue*>(nullptr);
			}
			return op.getContext()->createConstantValue(type, left % right, name);
		};

	evaluate["bit.and"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<BitVector, std::bit_and<BitVector>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["bit.xor"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<BitVector, std::bit_xor<BitVector>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["bit.or"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<BitVector, std::bit_or<BitVector>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["bit.not"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeUnaryOp<BitVector, std::bit_not<BitVector>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.lt"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::less<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Integer.le"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::less_equal<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Integer.gt"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::greater<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Integer.ge"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::greater_equal<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Integer.eq"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::equal_to<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Integer.ne"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			bool relation = computeRelationOp<Integer, std::not_equal_to<Integer>>(src);
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(relation), name);
		};

	evaluate["Logic.and"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			assert(!src.empty());
			bool result = std::find_if(src.begin(), src.end(),
				[](ConstantValue* value) { return !value->getValue<Logic>().isTrue(); })
				== src.end();
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(result), name);
		};

	evaluate["Logic.or"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			assert(!src.empty());
			bool result = std::find_if(src.begin(), src.end(),
				[](ConstantValue* value) { return value->getValue<Logic>().isTrue(); })
				!= src.end();
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(result), name);
		};

	evaluate["Logic.not"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			assert(src.size() == 1);
			bool result = !src.front()->getValue<Logic>().isTrue();
			return op.getContext()->createConstantValue(op.getReturnType(), op.getContext()->getLogic(result), name);
		};

	evaluate["Integer.max"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<Integer, std::function<Integer(Integer, Integer)>>(src,
				[](Integer a, Integer b) { return a > b ? a : b; });
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.min"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeReduceOp<Integer, std::function<Integer(Integer, Integer)>>(src,
				[](Integer a, Integer b) { return a < b ? a : b; });
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};

	evaluate["Integer.abs"] = [](Operation op, const std::vector<ConstantValue*>& src, String name)
		{
			auto value = computeUnaryOp<Integer, std::function<Integer(Integer)>>(src,
				[](Integer a) { return a.sign() >= 0 ? a : -a; });
			return op.getContext()->createConstantValue(op.getReturnType(), value, name);
		};
}
