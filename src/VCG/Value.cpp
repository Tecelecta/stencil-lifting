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
			throw std::runtime_error("Src operand if nullptr");
		}
		if (auto var_dependeny = dynamic_cast<VariableValue*>(dependeny))
		{
			if (var_dependeny->getSection() != this->getSection())
			{
				throw std::runtime_error("Parent section of src must be consistant with OperationValue");
			}
		}
		if (!dependeny->getType().canBeArgument(op.getParameterType(i)))
		{
			std::cerr << dependeny->getType().toString() << std::endl;
			std::cerr << op.toString() << std::endl;
			std::cerr << op.getParameterType(i).toString() << std::endl;
			throw std::runtime_error("Operand type do not match operation type");
		}
	}
	if (!op.getReturnType().canBeArgument(type))
	{
		throw std::runtime_error("OpCode do not match result type");
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
		throw std::runtime_error("Src operand if nullptr");
	}
	if (!src->getType().canBeArgument(type))
	{
		throw std::runtime_error("Src data type do not match result data type");
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
			throw std::runtime_error("Src operand if nullptr");
		}
		if (auto var_dependeny = dynamic_cast<VariableValue*>(dependeny))
		{
			if (var_dependeny->getSection() != this->getSection())
			{
				throw std::runtime_error("Parent section of incoming value must be consistant with PHI value");
			}
		}
		if (!dependeny->getType().canBeArgument(type))
		{
			throw std::runtime_error("Src operand type do not match reuslt type");
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
