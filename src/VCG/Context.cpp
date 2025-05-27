#include "Context.h"

Context::Context() : GarbageCollector(this),
	String::Factory(this), Enum::Factory(this), BitVector::Factory(this), Integer::Factory(this), Decimal::Factory(this),
	Type::Factory(this), Operation::Factory(this)
{
	ConstantValue::Builder builder;
	trueValue = createConstantValue(getLogicType(), getTrue());
	trueValueGuard = MarkedObjectGuard(trueValue);
	falseValue = createConstantValue(getLogicType(), getFalse());
	falseValueGuard = MarkedObjectGuard(falseValue);
	zeroValue = createConstantValue(getIntegerType(), getZero());
	zeroValueGuard = MarkedObjectGuard(zeroValue);
}

Context::~Context()
{
	GarbageCollector::deleteAll();
}

ConstantValue* Context::createConstantValue(Type type, Symbol value, String name)
{
	ConstantValue::Builder builder;
	builder.createInstance(this);
	builder.setType(type);
	builder.setValue(value);
	builder.setName(name);
	return builder.getInstance();
}

InvalidValue* Context::createInvalidValue(Type type, String name)
{
	InvalidValue::Builder builder;
	builder.createInstance(this);
	builder.setType(type);
	builder.setName(name);
	return builder.getInstance();
}
