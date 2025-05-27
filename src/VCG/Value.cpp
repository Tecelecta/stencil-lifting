#define VCG_EXPORTS

#include "Section.h"

void Value::markReference()
{
	MarkedObject::markReference();
	name.markImpl();
	type.markImpl();
}

void VariableValue::markReference()
{
	Value::markReference();
	section->markSelf();
}

void OperationValue::validate() const
{
	for (size_t i = 0; i < srcVector.size(); i++)
	{
		auto dependeny = srcVector[i];
		if (dependeny == nullptr)
		{
			throw std::runtime_error("源操作数是nullptr");
		}
		if (auto var_dependeny = dynamic_cast<VariableValue*>(dependeny))
		{
			if (var_dependeny->getSection() != this->getSection())
			{
				throw std::runtime_error("操作值的源操作数所属的Section必须是和变量所属的Section一致");
			}
		}
		if (!dependeny->getType().canBeArgument(op.getParameterType(i)))
		{
			std::cerr << dependeny->getType().toString() << std::endl;
			std::cerr << op.toString() << std::endl;
			std::cerr << op.getParameterType(i).toString() << std::endl;
			throw std::runtime_error("源操作数与操作类型不匹配");
		}
	}
	if (!op.getReturnType().canBeArgument(type))
	{
		throw std::runtime_error("操作码类型与结果类型不匹配");
	}
}

void OperationValue::markReference()
{
	VariableValue::markReference();
	op.markImpl();
	for (auto object : srcVector)
	{
		object->markSelf();
	}
}

void ResultValue::markReference()
{
	VariableValue::markReference();
	src->markSelf();
	call->markSelf();
}

void ResultValue::validate() const
{
	if (src == nullptr)
	{
		throw std::runtime_error("源操作数是nullptr");
	}
	if (!src->getType().canBeArgument(type))
	{
		throw std::runtime_error("数据源类型与结果类型不匹配");
	}
}

void PhiValue::markReference()
{
	VariableValue::markReference();
	for (auto object : incomingVector)
	{
		object->markSelf();
	}
}

void PhiValue::validate() const
{
	for (size_t i = 0; i < incomingVector.size(); i++)
	{
		auto dependeny = incomingVector[i];
		if (dependeny == nullptr)
		{
			throw std::runtime_error("源操作数是nullptr");
		}
		if (auto var_dependeny = dynamic_cast<VariableValue*>(dependeny))
		{
			if (var_dependeny->getSection() != this->getSection())
			{
				throw std::runtime_error("合流值的入边所属的Section必须是和变量所属的Section一致");
			}
		}
		if (!dependeny->getType().canBeArgument(type))
		{
			throw std::runtime_error("源操作数与合流结果类型不匹配");
		}
	}
}

void ConstantValue::markReference()
{
	Value::markReference();
	value.markImpl();
}

InputValue* InputValue::Builder::initInstance(const Value* src)
{
	createInstance(src->getContext());
	copyType(src);
	copyName(src);
	return instance;
}

#define BUILDER_CONSTRUCT_FUNCTION(T) \
T* T::Builder::createInstance(Context* context) \
{ \
	return instance = new T(context); \
} \
T* T::Builder::cloneInstance(const T* src) \
{ \
	return instance = new T(*src); \
} \
Value::Builder T::initBuilder() const \
{ \
	T::Builder builder; \
	builder.createInstance(getContext()); \
	return builder; \
} \
Value::Builder T::cloneBuilder() const \
{ \
	T::Builder builder; \
	builder.cloneInstance(this); \
	return builder; \
}
BUILDER_CONSTRUCT_FUNCTION(InputValue)
BUILDER_CONSTRUCT_FUNCTION(BoundValue)
BUILDER_CONSTRUCT_FUNCTION(OperationValue)
BUILDER_CONSTRUCT_FUNCTION(ResultValue)
BUILDER_CONSTRUCT_FUNCTION(PhiValue)
BUILDER_CONSTRUCT_FUNCTION(ConstantValue)
BUILDER_CONSTRUCT_FUNCTION(InvalidValue)
