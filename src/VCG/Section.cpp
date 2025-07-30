#define VCG_EXPORTS

#include "Context.h"

void SectionCall::markReference()
{
	MarkedObject::markReference();
	callee->markSelf();
	for (auto object : argumentVector)
	{
		object->markSelf();
	}
	for (auto object : specializerVector)
	{
		object->markSelf();
	}
	for (auto object : resultValueVector)
	{
		object->markSelf();
	}
}

void SectionCall::validate() const
{
	if (caller == nullptr)
	{
		throw std::runtime_error("`caller` cannot be nullptr");
	}
	if (callee == nullptr)
	{
		throw std::runtime_error("`callee` cannot be nullptr");
	}
	for (auto value : argumentVector)
	{
		if (auto variableValue = dynamic_cast<VariableValue*>(value))
		{
			if (variableValue->getSection() != caller)
			{
				throw std::runtime_error("Arguments' parent section must be `caller`");
			}
		}
	}
	for (auto value : specializerVector)
	{
		if (auto variableValue = dynamic_cast<VariableValue*>(value))
		{
			if (variableValue->getSection() != caller)
			{
				throw std::runtime_error("Arguments' parent section must be `caller`");
			}
		}
	}
	for (auto value : resultValueVector)
	{
		if (value->getSection() != caller)
		{
			throw std::runtime_error("Result value's parent section must be `caller`");
		}
		if (value->getCall() != this)
		{
			throw std::runtime_error("Result value's parent SectionCall must be `this`");
		}
		if (value->getSrc()->getSection() != callee)
		{
			throw std::runtime_error("Result value's `callee` must be its parent section");
		}
	}
}

SectionCall* SectionCall::Builder::createInstance(Section* callee)
{
	instance = new SectionCall(callee->getContext());
	instance->callee = callee;
	instance->argumentVector.resize(callee->getParameterVectorSize(), nullptr);
	if (auto callee_SectionGenerator = dynamic_cast<SectionGenerator*>(callee))
	{
		instance->specializerVector.resize(callee_SectionGenerator->getTemplateVectorSize(), nullptr);
	}
	return instance;
}

SectionCall* SectionCall::Builder::cloneInstance(SectionCall* src)
{
	return instance = new SectionCall(*src);
}

void SectionCall::Builder::addResultValue(ResultValue::Builder& builder)
{
	instance->resultValueVector.push_back(builder.getInstance());
	builder.setCall(instance);
	builder.setSection(instance->caller);
}

ResultValue* SectionCall::Builder::createResultValue(VariableValue* src, String name)
{
	ResultValue::Builder builder;
	builder.createInstance(src->getContext());
	builder.setSrcAndType(src);
	builder.setName(name);
	addResultValue(builder);
	return builder.getInstance();
}

void Section::markReference()
{
	MarkedObject::markReference();
	for (auto object : parameterVector)
	{
		object->markSelf();
	}
	for (auto object : sectionCallVector)
	{
		object->markSelf();
	}
	sectionName.markImpl();
}

std::vector<VariableValue*> Section::getVariableVector() const
{
	std::vector<VariableValue*> result(parameterVector.begin(), parameterVector.end());
	for (auto call : sectionCallVector)
	{
		if (call != nullptr)
		{
			result.insert(result.end(), call->getResultValueVector().begin(), call->getResultValueVector().end());
		}
	}
	return result;
}

void Section::validate() const
{
	auto variableVector = getVariableVector();
	for (auto variableValue : variableVector)
	{
		if (variableValue == nullptr)
		{
			throw std::runtime_error("`value` cannot be nullptr");
		}
		if (variableValue->getSection() != this)
		{
			throw std::runtime_error("`value.section` of values in Graph Section must be `this`");
		}
		variableValue->validate();
	}
	for (auto call : sectionCallVector)
	{
		if (call == nullptr)
		{
			throw std::runtime_error("`call` cannot be nullptr");
		}
		if (call->getCaller() != this)
		{
			throw std::runtime_error("`caller` of SecionCall in Graph Section must be `this`");
		}
		call->validate();
	}
}

void SimpleSection::markReference()
{
	Section::markReference();
	for (auto object : operationValueVector)
	{
		object->markSelf();
	}
}

std::vector<VariableValue*> SimpleSection::getVariableVector() const
{
	auto result = Section::getVariableVector();
	result.insert(result.end(), operationValueVector.begin(), operationValueVector.end());
	return result;
}

Section::Builder SimpleSection::initBuilder() const
{
	Builder builder;
	builder.createInstance(getContext());
	return builder;
}

SimpleSection* SimpleSection::Builder::createInstance(Context* context)
{
	return instance = new SimpleSection(context);
}

OperationValue* SimpleSection::Builder::createOperationValue(Operation op, std::vector<Value*> srcVector, String name)
{
	OperationValue::Builder builder;
	builder.createInstance(instance->getContext());
	builder.setOperationAndType(op);
	builder.setSrcVector(std::move(srcVector));
	builder.setName(name);
	addOperationValue(builder);
	return builder.getInstance();
}

OperationValue* SimpleSection::Builder::createCopyValue(Type type, Value* src, String name)
{
	return createOperationValue(instance->getContext()->getCopyOperation(type), { src }, name);
}

OperationValue* SimpleSection::Builder::createSelectValue(Type type, Value* condition, Value* trueBranch, Value* falseBranch, String name)
{
	auto context = instance->getContext();
	assert(condition->getType() == context->getLogicType());
	OperationValue::Builder builder;
	builder.createInstance(context);
	builder.setOperationAndType(context->getBinarySelectOperation(type));
	builder.setSrcVector({ condition, trueBranch, falseBranch });
	builder.setName(name);
	addOperationValue(builder);
	return builder.getInstance();
}

OperationValue* SimpleSection::Builder::createArrayGetValue(Value* base, const std::vector<Value*>& index, String name)
{
	auto context = instance->getContext();
	auto arrayType = base->getType().cast<ArrayType>();
	assert(arrayType != nullptr);
	std::vector<Value*> srcVector = { base };
	for (auto indexValue : index)
	{
		srcVector.push_back(indexValue);
	}
	return createOperationValue(context->getArrayGetOperation(arrayType), srcVector, name);
}

OperationValue* SimpleSection::Builder::createArraySetValue(Value* base, const std::vector<Value*>& index, Value* val, String name)
{
	auto context = instance->getContext();
	auto arrayType = base->getType().cast<ArrayType>();
	assert(arrayType != nullptr);
	std::vector<Value*> srcVector = { base };
	for (auto indexValue : index)
	{
		srcVector.push_back(indexValue);
	}
	srcVector.push_back(val);
	return createOperationValue(context->getArraySetOperation(arrayType), srcVector, name);
}

OperationValue* SimpleSection::Builder::createArrayLowerBoundValue(Value* base, Value* index, String name)
{
	auto context = instance->getContext();
	auto arrayType = base->getType().cast<ArrayType>();
	assert(arrayType != nullptr);
	return createOperationValue(context->getArrayLowerBoundOperation(arrayType), { base, index }, name);
}

OperationValue* SimpleSection::Builder::createArrayUpperBoundValue(Value* base, Value* index, String name)
{
	auto context = instance->getContext();
	auto arrayType = base->getType().cast<ArrayType>();
	assert(arrayType != nullptr);
	return createOperationValue(context->getArrayUpperBoundOperation(arrayType), { base, index }, name);
}

OperationValue* SimpleSection::Builder::createTupleGetValue(Value* base, size_t index, String name)
{
	auto context = instance->getContext();
	auto tupleType = base->getType().cast<TupleType>();
	assert(tupleType != nullptr);
	std::vector<Value*> srcVector = { base,
		context->createConstantValue(context->getIntegerType(), context->getInteger(index)) };
	return index < tupleType.getElementNum() ?
		createOperationValue(context->getTupleGetOperation(tupleType, index), srcVector, name) :
		nullptr;
}

OperationValue* SimpleSection::Builder::createTupleSetValue(Value* base, size_t index, Value* val, String name)
{
	auto context = instance->getContext();
	auto tupleType = base->getType().cast<TupleType>();
	assert(tupleType != nullptr);
	std::vector<Value*> srcVector = { base,
		context->createConstantValue(context->getIntegerType(), context->getInteger(index)),
		val };
	return index < tupleType.getElementNum() ?
		createOperationValue(context->getTupleSetOperation(tupleType, index), srcVector, name) :
		nullptr;
}

OperationValue* SimpleSection::Builder::createStructGetValue(Value* base, String elem, String name)
{
	auto structType = base->getType().cast<StructType>();
	assert(structType != nullptr);
	auto index = structType.getIndexByName(elem);
	return index.has_value() ? createTupleGetValue(base, index.value(), name) : nullptr;
}

OperationValue* SimpleSection::Builder::createStructSetValue(Value* base, String elem, Value* val, String name)
{
	auto structType = base->getType().cast<StructType>();
	assert(structType != nullptr);
	auto index = structType.getIndexByName(elem);
	return index.has_value() ? createTupleSetValue(base, structType.getIndexByName(elem).value(), val, name) : nullptr;
}

void SectionGenerator::markReference()
{
	Section::markReference();
	for (auto object : templateVector)
	{
		object->markSelf();
	}
	for (auto object : boundVector)
	{
		object->markSelf();
	}
	for (auto object : phiValueVector)
	{
		object->markSelf();
	}
}

std::vector<VariableValue*> SectionGenerator::getVariableVector() const
{
	auto result = Section::getVariableVector();
	result.insert(result.end(), templateVector.begin(), templateVector.end());
	result.insert(result.end(), boundVector.begin(), boundVector.end());
	result.insert(result.end(), phiValueVector.begin(), phiValueVector.end());
	return result;
}

Section::Builder BinaryBranch::initBuilder() const
{
	Builder builder;
	builder.createInstance(getContext());
	return builder;
}

BinaryBranch* BinaryBranch::Builder::createInstance(Context* context)
{
	instance = new BinaryBranch(context);
	setTemplateVectorSize(1);
	setSectionCallVectorSize(2);

	// 设置条件操作数形参
	InputValue::Builder builder;
	builder.createInstance(context);
	builder.setType(context->getLogicType());
	builder.setName(context->getString("condition"));
	setTemplate(0, builder);

	return instance;
}

PhiValue* BinaryBranch::Builder::createPhiValue(Type type, Value* ifValue, Value* elseValue, String name)
{
	PhiValue::Builder builder;
	builder.createInstance(this->instance->getContext());
	builder.setType(type);
	builder.setIncomingVector({ ifValue, elseValue });
	builder.setName(name);
	addPhiValue(builder);
	return builder.getInstance();
}

Section::Builder IterateLoop::initBuilder() const
{
	Builder builder;
	builder.createInstance(getContext());
	return builder;
}

IterateLoop* IterateLoop::Builder::createInstance(Context* context)
{
	instance = new IterateLoop(context);
	setTemplateVectorSize(3);
	setBoundVectorSize(1);
	setSectionCallVectorSize(1);

	InputValue::Builder inputBuilder;
	auto integer_type = context->getIntegerType();

	// 设置循环计数器开始下标形参
	inputBuilder.createInstance(context);
	inputBuilder.setType(integer_type);
	inputBuilder.setName(context->getString("begin"));
	setTemplate(0, inputBuilder);

	// 设置循环迭代次数形参
	inputBuilder.createInstance(context);
	inputBuilder.setType(integer_type);
	inputBuilder.setName(context->getString("times"));
	setTemplate(1, inputBuilder);

	// 设置循环计数器步长形参
	inputBuilder.createInstance(context);
	inputBuilder.setType(integer_type);
	inputBuilder.setName(context->getString("step"));
	setTemplate(2, inputBuilder);

	// 设置循环计数器约束变元
	BoundValue::Builder boundBuilder;
	boundBuilder.createInstance(context);
	boundBuilder.setType(integer_type);
	boundBuilder.setName(context->getString("counter"));
	setBound(0, boundBuilder);

	return instance;
}

PhiValue* IterateLoop::Builder::createPhiValue(Type type, Value* initValue, Value* loopValue, String name)
{
	PhiValue::Builder builder;
	builder.createInstance(this->instance->getContext());
	builder.setType(type);
	builder.setIncomingVector({ initValue, loopValue });
	builder.setName(name);
	addPhiValue(builder);
	return builder.getInstance();
}

Section::Builder ParallelLoop::initBuilder() const
{
	Builder builder;
	builder.createInstance(getContext(), getCounterNum());
	return builder;
}

ParallelLoop* ParallelLoop::Builder::createInstance(Context* context, size_t counterNum)
{
	instance = new ParallelLoop(context);
	setBoundVectorSize(counterNum);
	setSectionCallVectorSize(1);

	BoundValue::Builder boundBuilder;
	auto integer_type = context->getIntegerType();
	for (size_t i = 0; i < counterNum; i++)
	{
		// 设置约束变元
		boundBuilder.createInstance(context);
		boundBuilder.setType(integer_type);
		boundBuilder.setName(context->getString("counter_" + std::to_string(i)));
		setBound(i, boundBuilder);
	}
	return instance;
}

PhiValue* ParallelLoop::Builder::createPhiValue(Type type, Value* initValue, Value* loopValue, String name)
{
	assert(type.cast<ArrayType>() != nullptr);
	PhiValue::Builder builder;
	builder.createInstance(this->instance->getContext());
	builder.setType(type);
	builder.setIncomingVector({ initValue, loopValue });
	builder.setName(name);
	addPhiValue(builder);
	return builder.getInstance();
}
