#define VCG_EXPORTS

#include "Context.h"
#include "ContainerHash.h"

#include <boost/pfr.hpp>

BOOST_PFR_HASH_EQ_NE_API(Operation::Overload)
BOOST_PFR_HASH_EQ_NE_API(Operation::Data)

void Operation::Impl::markReference()
{
	Symbol::Impl::markReference();
	name.markImpl();
	for (auto& type : parameterTypes)
	{
		type.markImpl();
	}
	returnType.markImpl();
}

void Operation::Factory::eraseUseless()
{
	for (auto iter = objectPool.begin(); iter != objectPool.end();)
	{
		if (!iter->second.checkMarked())
		{
			objectPool.erase(iter++);
			continue;
		}
		++iter;
	}
}

Operation Operation::Factory::getOperation(Operation::Data data)
{
	auto iter = objectPool.find(data);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new Operation::Impl(context, data);
		iter = objectPool.emplace(std::move(data), impl_ptr).first;
	}
	return iter->second;
}

Operation Operation::Factory::getOperation(Operation::Overload name_and_parameterTypes, Type returnType)
{
	return getOperation({ name_and_parameterTypes.name, name_and_parameterTypes.parameterTypes, returnType });
}

Operation Operation::Factory::getOperation(String name, const std::vector<Type>& parameterTypes, Type returnType)
{
	return getOperation({ name, parameterTypes, returnType });
}

Operation Operation::Factory::getCopyOperation(Type type)
{
	return getOperation(context->getString("copy"), { type }, type);
}

Operation Operation::Factory::getBinarySelectOperation(Type type)
{
	return getOperation(context->getString("select"), { context->getLogicType(), type, type }, type);
}

Operation Operation::Factory::getArrayGetOperation(ArrayType arrayType)
{
	std::vector<Type> parameterTypes(arrayType.getNumDims() + 1, context->getIntegerType());
	parameterTypes.front() = arrayType;
	return getOperation(context->getString("array.getAt"), parameterTypes, arrayType.getElementType());
}

Operation Operation::Factory::getArraySetOperation(ArrayType arrayType)
{
	std::vector<Type> parameterTypes(arrayType.getNumDims() + 2, context->getIntegerType());
	parameterTypes.front() = arrayType;
	parameterTypes.back() = arrayType.getElementType();
	return getOperation(context->getString("array.setAt"), parameterTypes, arrayType);
}

Operation Operation::Factory::getArrayLowerBoundOperation(ArrayType arrayType)
{
	std::vector<Type> parameterTypes = { arrayType, context->getIntegerType() };
	return getOperation(context->getString("array.lowerBound"), parameterTypes, context->getIntegerType());
}

Operation Operation::Factory::getArrayUpperBoundOperation(ArrayType arrayType)
{
	std::vector<Type> parameterTypes = { arrayType, context->getIntegerType() };
	return getOperation(context->getString("array.upperBound"), parameterTypes, context->getIntegerType());
}

Operation Operation::Factory::getTupleGetOperation(TupleType tupleType, size_t index)
{
	return getOperation(context->getString("tuple.getAt"), { tupleType, context->getIntegerType() }, tupleType.getElementType(index));
}

Operation Operation::Factory::getTupleSetOperation(TupleType tupleType, size_t index)
{
	return getOperation(context->getString("tuple.setAt"), { tupleType, context->getIntegerType(), tupleType.getElementType(index) }, tupleType);
}
