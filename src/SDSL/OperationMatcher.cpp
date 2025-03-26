#include "OperationMatcher.h"

#include "VCG/Context.h"

#define ERROR(message) errorHandler.error(message, opLine, opColumn); return Operation();

static bool isIntType(Type type, size_t& bits)
{
	if (type.getName().equals("int"))
	{
		bits = type.cast<TemplateType>().getArgument(0).cast<Integer>().getData().convert_to<size_t>();
		return true;
	}
	return false;
}

static bool isFloatType(Type type, size_t& bits)
{
	if (type.getName().equals("float"))
	{
		bits = 1
				+ type.cast<TemplateType>().getArgument(0).cast<Integer>().getData().convert_to<size_t>()
				+ type.cast<TemplateType>().getArgument(1).cast<Integer>().getData().convert_to<size_t>();
		return true;
	}
	return false;
}

Operation OperationMatcher::matchBasicTypeConstructor(
	Context* context, bool& isBasicType,
	Type dstType, Type srcType,
	size_t opLine, size_t opColumn)
{
	// 相等、继承、协变的情况在外部有判断 
	size_t srcBits = 0;
	size_t dstBits = 0;
	if (srcType == context->getIntegerType())
	{
		if (isIntType(dstType, dstBits))
		{
			if (dstBits == 8 || dstBits == 16 || dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("int.trunc"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持的目标整数类型")
		}
		else if (isFloatType(dstType, dstBits))
		{
			if (dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("int.2float"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持的目标浮点数类型")
		}
		ERROR("在进行类型转换时，不支持当前目标类型")
	}
	else if (srcType == context->getRealType())
	{
		if (isIntType(dstType, dstBits))
		{
			if (dstBits == 8 || dstBits == 16 || dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("float.2int"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持的目标整数类型")
		}
		else if (isFloatType(dstType, dstBits))
		{
			if (dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("float.trunc"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持的目标浮点数类型")
		}
		ERROR("在进行类型转换时，不支持当前目标类型")
	}
	else if (isIntType(srcType, srcBits))
	{
		if (isIntType(dstType, dstBits))
		{
			if (dstBits == 8 || dstBits == 16 || dstBits == 32 || dstBits == 64)
			{
				if (dstBits < srcBits)
				{
					return context->getOperation(context->getString("int.trunc"), { srcType }, dstType);
				}
				else if (dstBits > srcBits)
				{
					return context->getOperation(context->getString("int.sext"), { srcType }, dstType);
				}
				return context->getCopyOperation(dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持将当前类型的整数转换为目标整数类型")
		}
		else if (isFloatType(dstType, dstBits))
		{
			if (dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("int.2float"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持将当前类型的整数转换为目标浮点数类型")
		}
		ERROR("在进行类型转换时，不支持当前目标类型")
	}
	else if (isFloatType(srcType, srcBits))
	{
		if (isIntType(dstType, dstBits))
		{
			if (dstBits == 8 || dstBits == 16 || dstBits == 32 || dstBits == 64)
			{
				return context->getOperation(context->getString("float.2int"), { srcType }, dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持将当前类型的浮点数转换为目标整数类型")
		}
		else if (isFloatType(dstType, dstBits))
		{
			if (dstBits == 32 || dstBits == 64)
			{
				if (dstBits < srcBits)
				{
					return context->getOperation(context->getString("float.trunc"), { srcType }, dstType);
				}
				else if (dstBits > srcBits)
				{
					return context->getOperation(context->getString("float.ext"), { srcType }, dstType);
				}
				return context->getCopyOperation(dstType);
			}
			isBasicType = false;
			ERROR("在进行类型转换时，不支持将当前类型的浮点数转换为目标浮点数类型")
		}
		ERROR("在进行类型转换时，不支持当前目标类型")
	}
	else
	{
		isBasicType = false;
		ERROR("在进行类型转换时，不支持当前源操作数类型")
	}
}

Operation OperationMatcher::matchIntrinsicFunction(
	Token token, Context* context,
	const std::vector<Type>& srcType, const std::vector<bool>& isImm)
{
	// TODO: 实现重载匹配
	bool all_integer = !srcType.empty();
	for (const auto& type : srcType)
	{
		if (type != context->getIntegerType())
		{
			all_integer = false;
			break;
		}
	}
	if (all_integer)
	{
		return context->getOperation(context->getString(std::string("Integer.") + std::string(token.text)),
			srcType, context->getIntegerType());
	}
	else
	{
		return context->getOperation(context->getString(std::string("Real.") + std::string(token.text)),
			srcType, context->getRealType());
	}
}

Operation OperationMatcher::matchOperatorExpression(
	Token token, Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast)
{
#define __args__ context, leftType, rightType, leftIsImm, rightIsImm, leftCast, rightCast, token.line, token.column

	switch (token.category)
	{
	case Token::Category::ASG:
		return matchAssignmentOperation("copy", __args__);

	// Arithmetic Operation
	case Token::Category::ADD:
		// TODO: 代数类型找继承的公共祖先
		// 隐式转换，int2fp，短2长
		return matchBinaryArithmeticOperation("add", "add", "add", __args__);
	case Token::Category::SUB:
		return leftType == nullptr ?
			matchUnaryArithmeticOperation("neg", context, rightType, token.line, token.column) :
			matchBinaryArithmeticOperation("sub", "sub", "sub", __args__);
	case Token::Category::MUL:
		return matchBinaryArithmeticOperation("mul", "mul", "mul", __args__);
	case Token::Category::DIV:
		return matchBinaryArithmeticOperation("udiv", "sdiv", "div", __args__);
	case Token::Category::MOD:
		return matchBinaryArithmeticOperation("umod", "smod", "mod", __args__);
	case Token::Category::POW:
		return matchBinaryArithmeticOperation("pow", "pow", "pow", __args__);

		// Logical Operation
	case Token::Category::NOT:
		return matchUnaryLogicalOperation("Logic.not", context, rightType, token.line, token.column);
	case Token::Category::AND:
		return matchBinaryLogicalOperation("Logic.and", __args__);
	case Token::Category::OR:
		return matchBinaryLogicalOperation("Logic.or", __args__);

		// Bitwise Binary Operation
	case Token::Category::BIT_AND:
		return matchBinaryBitwiseOperation("bit.and", __args__);
	case Token::Category::BIT_OR:
		return matchBinaryBitwiseOperation("bit.or", __args__);
	case Token::Category::BIT_XOR:
		return matchBinaryBitwiseOperation("bit.xor", __args__);
	case Token::Category::BIT_NOT:
		return matchUnaryBitwiseOperation("bit.not", context, rightType, token.line, token.column);

	case Token::Category::SHL:
		return matchShiftOperation(false, "int.shl", __args__);
	case Token::Category::ASHR:
		return matchShiftOperation(false, "int.ashl", __args__);

		// Relational Operation
	case Token::Category::EQUAL:
		return matchRelationalOperation("ueq", "seq", "eq", __args__);
	case Token::Category::NOT_EQUAL:
		return matchRelationalOperation("une", "sne", "ne", __args__);
	case Token::Category::LES:
		return matchRelationalOperation("ult", "slt", "lt", __args__);
	case Token::Category::GRT:
		return matchRelationalOperation("ugt", "sgt", "gt", __args__);
	case Token::Category::LES_EQUAL:
		return matchRelationalOperation("ule", "sle", "le", __args__);
	case Token::Category::GRT_EQUAL:
		return matchRelationalOperation("uge", "sge", "ge", __args__);

		// Assignment Operation
	case Token::Category::ADD_ASG:
		return matchArithmeticAssignmentOperation("add", "add", "add", __args__);
	case Token::Category::SUB_ASG:
		return matchArithmeticAssignmentOperation("sub", "sub", "sub", __args__);
	case Token::Category::MUL_ASG:
		return matchArithmeticAssignmentOperation("mul", "mul", "mul", __args__);
	case Token::Category::DIV_ASG:
		return matchArithmeticAssignmentOperation("udiv", "sdiv", "div", __args__);
	case Token::Category::MOD_ASG:
		return matchArithmeticAssignmentOperation("umod", "smod", "mod", __args__);
	case Token::Category::BIT_AND_ASG:
		return matchBitwiseAssignmentOperation("bit.and", __args__);
	case Token::Category::BIT_OR_ASG:
		return matchBitwiseAssignmentOperation("bit.or", __args__);
	case Token::Category::BIT_XOR_ASG:
		return matchBitwiseAssignmentOperation("bit.xor", __args__);
	case Token::Category::SHL_ASG:
		return matchShiftOperation(true, "int.shl", __args__);
	case Token::Category::ASHR_ASG:
		return matchShiftOperation(true, "int.ashl", __args__);

	default:
		assert(false);
	}
	return Operation();

#undef __args__
}

Operation OperationMatcher::matchAssignmentOperation(
	std::string op, Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	if (rightType.canBeArgument(leftType))
	{
		// 相等、继承、协变都可以直接拷贝
		return context->getCopyOperation(leftType);
	}
	size_t leftBits = 0;
	size_t rightBits = 0;
	if (leftIsImm)
	{
		ERROR("在进行赋值运算时，不支持对立即数进行赋值")
	}
	if (isIntType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			if (leftBits > rightBits)
			{
				// 有符号整型符号扩展，暂时不写无符号整型
				return context->getOperation(context->getString("int.sext"), { rightType }, leftType);
			}
			return context->getOperation(context->getString("int.trunc"), { rightType }, leftType);
		}
		else if (isFloatType(rightType, rightBits))
		{
			return context->getOperation(context->getString("float.2int"), { rightType }, leftType);
		}
		else if (rightIsImm)
		{
			return context->getCopyOperation(leftType);
		}
		ERROR("在进行赋值运算时，不支持将当前类型赋给整型操作数")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			return context->getOperation(context->getString("int.2float"), { rightType }, leftType);
		}
		else if (isFloatType(rightType, rightBits))
		{
			if (leftBits > rightBits)
			{
				return context->getOperation(context->getString("float.ext"), { rightType }, leftType);
			}
			return context->getOperation(context->getString("float.trunc"), { rightType }, leftType);
		}
		else if (rightIsImm)
		{
			return context->getOperation(context->getString("float.trunc"), { rightType }, leftType);
		}
		ERROR("在进行赋值运算时，不支持将当前类型赋给浮点型操作数")
	}
	ERROR("在进行赋值运算时，不支持当前类型的操作数")
	// TODO: 考虑integer promotion
}

Operation OperationMatcher::matchUnaryArithmeticOperation(
	std::string op, Context* context,
	Type rightType,
	size_t opLine, size_t opColumn)
{
	size_t rightBits;
	if (isIntType(rightType, rightBits))
	{
		return context->getOperation(context->getString("int." + op), { rightType }, rightType);
	}
	else if (isFloatType(rightType, rightBits))
	{
		return context->getOperation(context->getString("float." + op), { rightType }, rightType);
	}
	else if (rightType == context->getIntegerType())
	{
		return context->getOperation(context->getString("Integer." + op), { rightType }, rightType);
	}
	else if (rightType == context->getRealType())
	{
		return context->getOperation(context->getString("Real." + op), { rightType }, rightType);
	}
	ERROR("在进行算术运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchBinaryArithmeticOperation(
	std::string uop, std::string sop, std::string op,
	Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	if (leftType == context->getIntegerType())
	{
		if (rightType == context->getIntegerType())
		{
			return context->getOperation(context->getString("Integer." + op), { leftType, rightType }, rightType);
		}
		else if (rightType == context->getRealType())
		{
			return context->getOperation(context->getString("Real." + op), { leftType, rightType }, rightType);
		}
	}
	else if (leftType == context->getRealType())
	{
		if (rightType == context->getIntegerType() ||
			rightType == context->getRealType())
		{
			return context->getOperation(context->getString("Real." + op), { leftType, rightType }, leftType);
		}
	}

	size_t leftBits = 0;
	size_t rightBits = 0;
	if (leftType == context->getIntegerType())
	{
		if (rightType == context->getIntegerType())
		{
			return context->getOperation(context->getString("Integer." + op), { leftType, rightType }, leftType);
		}
		if (leftIsImm)
		{
			if (isIntType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Integer.2int"), { leftType }, rightType);
				return context->getOperation(context->getString("int." + sop), { rightType, rightType }, rightType);
			}
			else if (isFloatType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Integer.2float"), { leftType }, rightType);
				return context->getOperation(context->getString("float." + op), { rightType, rightType }, rightType);
			}
		}
		ERROR("在进行算术运算时，不支持将大整型和当前类型的操作数进行运算")
	}
	else if (leftType == context->getRealType())
	{
		if (rightType.canBeArgument(leftType))
		{
			return context->getOperation(context->getString("Real." + op), { leftType, rightType }, leftType);
		}
		if (leftIsImm)
		{
			if (isFloatType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Real.2float"), { leftType }, rightType);
				return context->getOperation(context->getString("float." + op), { rightType, rightType }, rightType);
			}
		}
		ERROR("在进行算术运算时，不支持将实型和当前类型的操作数进行运算")
	}
	else if (isIntType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString("int." + sop), { leftType, rightType }, leftType);
			}
			else if (leftBits > rightBits)
			{
				rightCast = context->getOperation(context->getString("int.sext"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			leftCast = context->getOperation(context->getString("int.sext"), { leftType }, rightType);
			return context->getOperation(context->getString("int." + sop), { rightType, rightType }, rightType);
		}
		else if (isFloatType(rightType, rightBits))
		{
			leftCast = context->getOperation(context->getString("int.2float"), { leftType }, rightType);
			return context->getOperation(context->getString("float." + op), { rightType, rightType }, rightType);
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			else if (rightType == context->getRealType())
			{
				rightCast = context->getOperation(context->getString("Real.2int"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			ERROR("在进行算术运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行算术运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isFloatType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString("float." + op), { leftType, rightType }, leftType);
			}
			else if (leftBits > rightBits)
			{
				rightCast = context->getOperation(context->getString("float.ext"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			leftCast = context->getOperation(context->getString("float.ext"), { leftType }, rightType);
			return context->getOperation(context->getString("float." + op), { rightType, rightType }, rightType);
		}
		else if (isIntType(rightType, rightBits))
		{
			rightCast = context->getOperation(context->getString("int.2float"), { rightType }, leftType);
			return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2float"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			else if (rightType == context->getRealType())
			{
				rightCast = context->getOperation(context->getString("Real.2float"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			ERROR("在进行算术运算时，不支持将浮点型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行算术运算时，不支持将浮点型操作数和当前类型的操作数进行运算")
	}
	ERROR("在进行算术运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchUnaryLogicalOperation(
	std::string op, Context* context,
	Type rightType,
	size_t opLine, size_t opColumn)
{
	if (rightType == context->getLogicType())
	{
		return context->getOperation(context->getString(op), { rightType }, context->getLogicType());
	}
	ERROR("在进行逻辑运算时，不支持非逻辑运算的操作数")
}

Operation OperationMatcher::matchBinaryLogicalOperation(
	std::string op, Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	if (leftType == context->getLogicType() && rightType == context->getLogicType())
	{
		return context->getOperation(context->getString(op), { leftType, rightType }, context->getLogicType());
	}
	ERROR("在进行逻辑运算时，不支持非逻辑运算的操作数")
}

Operation OperationMatcher::matchUnaryBitwiseOperation(
	std::string op, Context* context,
	Type rightType,
	size_t opLine, size_t opColumn)
{
	size_t rightBits = 0;
	if (isIntType(rightType, rightBits))
	{
		return context->getOperation(context->getString(op), { rightType }, rightType);
	}
	ERROR("在进行位运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchBinaryBitwiseOperation(
	std::string op, Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	size_t leftBits = 0;
	size_t rightBits = 0;
	if (leftIsImm)
	{
		if (leftType == context->getIntegerType())
		{
			if (rightType == context->getIntegerType())
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
			}
			else if (isIntType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Integer.2int"), { leftType }, rightType);
				return context->getOperation(context->getString(op), { rightType, rightType }, rightType);
			}
			ERROR("在进行位运算时，不支持将大整形立即数和当前类型的操作数进行运算")
		}
		ERROR("在进行位运算时，不支持当前类型的立即数")
	}
	else if (isIntType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
			}
			ERROR("在进行位运算时，操作数的位宽不匹配")
		}
		else if (isFloatType(rightType, rightBits)) // 掩码运算
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, rightType);
			}
			ERROR("在进行位运算时，操作数的位宽不匹配")
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { leftType }, rightType);
				return context->getOperation(context->getString(op), { leftType, leftType }, leftType);
			}
			ERROR("在进行位运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行位运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits)) // 掩码运算
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
			}
			ERROR("在进行位运算时，操作数的位宽不匹配")
		}
		ERROR("在进行位运算时，不支持将浮点型操作数和当前类型的操作数进行运算")
	}
	ERROR("在进行位运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchShiftOperation(
	bool asg, std::string op,
	Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	if (leftIsImm)
	{
		ERROR("在进行位移运算时，不支持对立即数进行赋值")
	}
	Type shiftType = context->getInt8Type();
	size_t leftBits = 0;
	size_t rightBits = 0;
	if (!asg && isIntType(leftType, leftBits))
	{
		if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { leftType }, shiftType);
				return context->getOperation(context->getString(op), { leftType, leftType }, leftType);
			}
			ERROR("在进行位移运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		else if (isIntType(rightType, rightBits))
		{
			if (rightBits > 8)
			{
				rightCast = context->getOperation(context->getString("int.trunc"), { rightType }, shiftType);
				return context->getOperation(context->getString(op), { leftType, shiftType }, leftType);
			}
			return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
		}
		ERROR("在进行位移运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	ERROR("在进行位移运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchRelationalOperation( // TODO 两边都是Imm->RealType ; a < 1.5 -> a <= 1
	std::string uop, std::string sop, std::string op,
	Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	if (leftType == context->getIntegerType())
	{
		if (rightType == context->getIntegerType())
		{
			return context->getOperation(context->getString("Integer." + op), { leftType, rightType }, context->getLogicType());
		}
		else if (rightType == context->getRealType())
		{
			return context->getOperation(context->getString("Real." + op), { leftType, rightType }, context->getLogicType());
		}
	}
	else if (leftType == context->getRealType())
	{
		if (rightType == context->getIntegerType() ||
			rightType == context->getRealType())
		{
			return context->getOperation(context->getString("Real." + op), { leftType, rightType }, context->getLogicType());
		}
	}

	size_t leftBits = 0;
	size_t rightBits = 0;
	Type logicType = context->getLogicType();
	if (isIntType(leftType, leftBits))
	{
		if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, logicType);
			}
			ERROR("在进行关系运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		else if (isIntType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, logicType);
			}
			ERROR("在进行关系运算时，操作数的位宽不匹配")
		}
		ERROR("在进行关系运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isFloatType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString("float." + op), { leftType, rightType }, logicType);
			}
			ERROR("在进行关系运算时，操作数的位宽不匹配")
		}
		else if (rightIsImm)
		{
			if (rightType == context->getRealType())
			{
				rightCast = context->getOperation(context->getString("Real.2float"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, logicType);
			}
			ERROR("在进行关系运算时，不支持将浮点型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行关系运算时，不支持将浮点型操作数和当前类型的操作数进行运算")
	}
	else if (leftIsImm)
	{
		if (leftType == context->getIntegerType())
		{
			if (rightType == context->getIntegerType())
			{
				return context->getOperation(context->getString("Integer." + op), { leftType, rightType }, logicType);
			}
			else if (isIntType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Integer.2int"), { leftType }, rightType);
				return context->getOperation(context->getString("int." + sop), { rightType, rightType }, logicType);
			}
			ERROR("在进行比较运算时，不支持将大整形立即数和当前类型的操作数进行运算")
		}
		else if (leftType == context->getRealType())
		{
			if (rightType == context->getRealType())
			{
				return context->getOperation(context->getString("Real." + op), { leftType, rightType }, logicType);
			}
			else if (isFloatType(rightType, rightBits))
			{
				leftCast = context->getOperation(context->getString("Real.2float"), { leftType }, rightType);
				return context->getOperation(context->getString("float." + op), { rightType, rightType }, logicType);
			}
			ERROR("在进行比较运算时，不支持将实型立即数和当前类型的操作数进行运算")
		}
		ERROR("在进行关系运算时，不支持将当前类型的立即数和操作数进行运算")
	}
	ERROR("在进行关系运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchArithmeticAssignmentOperation(
	std::string uop, std::string sop, std::string op,
	Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	size_t leftBits = 0;
	size_t rightBits = 0;
	if (leftIsImm)
	{
		ERROR("在进行赋值运算时，不支持对立即数进行赋值")
	}
	if (leftType == context->getRealType())
	{
		if (rightType.canBeArgument(leftType))
		{
			return context->getOperation(context->getString("Real." + op), { leftType, leftType }, leftType);
		}
	}
	else if (leftType == context->getIntegerType())
	{
		if (rightType.canBeArgument(leftType))
		{
			return context->getOperation(context->getString("Integer." + op), { leftType, leftType }, leftType);
		}
	}
	else if (isIntType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString("int." + sop), { leftType, rightType }, leftType);
			}
			else if (leftBits > rightBits)
			{
				rightCast = context->getOperation(context->getString("int.sext"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			leftCast = context->getOperation(context->getString("int.trunc"), { rightType }, leftType);
			return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
		}
		else if (isFloatType(rightType, rightBits))
		{
			rightCast = context->getOperation(context->getString("float.2int"), { rightType }, leftType);
			return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			else if (rightType == context->getRealType())
			{
				rightCast = context->getOperation(context->getString("Real.2int"), { rightType }, leftType);
				return context->getOperation(context->getString("int." + sop), { leftType, leftType }, leftType);
			}
			ERROR("在进行算术运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行算术运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			rightCast = context->getOperation(context->getString("int.2float"), { rightType }, leftType);
			return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
		}
		else if (isFloatType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString("float." + op), { leftType, rightType }, leftType);
			}
			else if (leftBits > rightBits)
			{
				rightCast = context->getOperation(context->getString("float.ext"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			leftCast = context->getOperation(context->getString("float.trunc"), { rightType }, leftType);
			return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2float"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			else if (rightType == context->getRealType())
			{
				rightCast = context->getOperation(context->getString("Real.2float"), { rightType }, leftType);
				return context->getOperation(context->getString("float." + op), { leftType, leftType }, leftType);
			}
			ERROR("在进行算术运算时，不支持将浮点型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行算术运算时，不支持将浮点型操作数和当前类型的操作数进行运算")
	}
	ERROR("在进行算术运算时，不支持当前类型的操作数")
}

Operation OperationMatcher::matchBitwiseAssignmentOperation(
	std::string op, Context* context,
	Type leftType, Type rightType,
	bool leftIsImm, bool rightIsImm,
	Operation& leftCast, Operation& rightCast,
	size_t opLine, size_t opColumn)
{
	size_t leftBits = 0;
	size_t rightBits = 0;
	if (leftIsImm)
	{
		ERROR("在进行赋值运算时，不支持对立即数进行赋值")
	}
	else if (isIntType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits))
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
			}
			ERROR("在进行位运算时，操作数的位宽不匹配")
		}
		else if (rightIsImm)
		{
			if (rightType == context->getIntegerType())
			{
				rightCast = context->getOperation(context->getString("Integer.2int"), { leftType }, rightType);
				return context->getOperation(context->getString(op), { leftType, leftType }, leftType);
			}
			ERROR("在进行位运算时，不支持将整型操作数和当前类型的立即数进行运算")
		}
		ERROR("在进行位运算时，不支持将整型操作数和当前类型的操作数进行运算")
	}
	else if (isFloatType(leftType, leftBits))
	{
		if (isIntType(rightType, rightBits)) // 掩码运算
		{
			if (leftBits == rightBits)
			{
				return context->getOperation(context->getString(op), { leftType, rightType }, leftType);
			}
			ERROR("在进行位运算时，操作数的位宽不匹配")
		}
		ERROR("在进行位运算时，不支持将浮点型操作数和当前类型的操作数进行运算")
	}
	ERROR("在进行位运算时，不支持当前类型的操作数")
}
