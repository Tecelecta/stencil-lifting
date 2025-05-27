#define VCG_EXPORTS

#include "Context.h"
#include "ContainerHash.h"

#include <boost/pfr.hpp>

BOOST_PFR_HASH_EQ_NE_API(TemplateType::Data)
BOOST_PFR_HASH_EQ_NE_API(ArrayType::Data)
BOOST_PFR_HASH_EQ_NE_API(TupleType::Data)
BOOST_PFR_HASH_EQ_NE_API(StructType::Data)
BOOST_PFR_HASH_EQ_NE_API(ArrayDim::Range)

size_t ArrayDim::hash() const
{
	size_t result = 0;
	for (const auto& x : data)
	{
		result ^= x.hash();
	}
	return result + data.size();
}

bool ArrayDim::operator==(const ArrayDim& other) const
{
	return this->data == other.data;
}

bool ArrayDim::operator!=(const ArrayDim& other) const
{
	return this->data != other.data;
}

void Type::Impl::markReference()
{
	Symbol::Impl::markReference();
	name.markImpl();
}

bool Type::inheritsFrom(Type other) const
{
	static const std::unordered_set<std::string> INHERIT_SET =
	{
		"Integer Rational", "Integer Real", "Integer Complex"
		"Rational Real", "Rational Complex", "Real Complex"
	};
	auto key = this->getName().str() + ' ' + other.getName().str();
	return INHERIT_SET.count(key) > 0;
}

size_t Type::getNumDims() const
{
	if (auto arrayType = cast<ArrayType>(); arrayType != nullptr)
	{
		return arrayType.getNumDims();
	}
	return 0;
}

Integer Type::getNumElements() const
{
	if (auto compositeType = cast<CompositeType>(); compositeType != nullptr)
	{
		if (auto tupleType = cast<TupleType>(); tupleType != nullptr)
		{
			return getContext()->getInteger(tupleType.getElementNum());
		}
		// TODO
		assert(false);
		return Integer();
	}
	else
	{
		return getContext()->getInteger(1);
	}
}

void TemplateType::Impl::markReference()
{
	Type::Impl::markReference();
	for (auto& object : argumentVector)
	{
		object.markImpl();
	}
}

ArrayDim::ArrayDim(size_t numDims)
{
	assert(numDims <= MAX_NUM_DIMS);
	data.resize(numDims);
}

ArrayDim::ArrayDim(const std::vector<Integer>& data)
{
	assert(data.size() <= MAX_NUM_DIMS);
	this->data.resize(data.size());
	for (size_t i = 0; i < data.size(); i++)
	{
		this->data[i] = { data[i].getContext()->getZero(), data[i] - data[i].getContext()->getInteger(1) };
	}
}

ArrayDim::ArrayDim(std::vector<Range> data)
{
	assert(data.size() <= MAX_NUM_DIMS);
	this->data = std::move(data);
}

std::string ArrayDim::toString() const
{
	std::stringstream ss;
	ss << '[';
	for (size_t i = 0; i < getNumDims(); i++)
	{
		if (data[i].lower == nullptr)
		{
			ss << '?';
		}
		else
		{
			ss << data[i].lower.getData();
		}
		ss << ':';
		if (data[i].upper == nullptr)
		{
			ss << '?';
		}
		else
		{
			ss << data[i].upper.getData();
		}
		if (i < getNumDims() - 1)
		{
			ss << ',';
		}
	}
	ss << ']';
	return ss.str();
}

bool ArrayType::Impl::isCovariant(Type other) const
{
	if (auto other_array = other.cast<ArrayType>(); other_array != nullptr)
	{
		return elementType.canBeArgument(other_array.getElementType());
	}
	return false;
}

std::string ArrayType::Impl::toString() const
{
	return elementType.toString() + dim.toString();
}

void ArrayType::Impl::markReference()
{
	CompositeType::Impl::markReference();
	elementType.markImpl();
	for (auto& d : dim.data)
	{
		d.lower.markImpl();
		d.upper.markImpl();
	}
}

bool TupleType::Impl::isCovariant(Type other) const
{
	if (auto other_tuple = other.cast<TupleType>(); other_tuple != nullptr)
	{
		if (elementTypes.size() != other_tuple.getElementNum())
		{
			return false;
		}
		for (size_t i = 0; i < elementTypes.size(); i++)
		{
			if (!elementTypes[i].canBeArgument(other_tuple.getElementType(i)))
			{
				return false;
			}
		}
		return true;
	}
	return false;
}

void TupleType::Impl::markReference()
{
	CompositeType::Impl::markReference();
	for (auto& object : elementTypes)
	{
		object.markImpl();
	}
}

void StructType::Impl::markReference()
{
	TupleType::Impl::markReference();
	for (auto& object : elementNames)
	{
		object.markImpl();
	}
}

Type::Factory::Factory(Context* context)
	: FlyweightObject::Factory(context)
{
}

void Type::Factory::eraseUseless()
{
	for (auto iter = simplePool.begin(); iter != simplePool.end();)
	{
		if (!iter->second.checkMarked())
		{
			simplePool.erase(iter++);
			continue;
		}
		++iter;
	}
	for (auto iter = arrayPool.begin(); iter != arrayPool.end();)
	{
		if (!iter->second.checkMarked())
		{
			arrayPool.erase(iter++);
			continue;
		}
		++iter;
	}
	for (auto iter = tuplePool.begin(); iter != tuplePool.end();)
	{
		if (!iter->second.checkMarked())
		{
			tuplePool.erase(iter++);
			continue;
		}
		++iter;
	}
	for (auto iter = structPool.begin(); iter != structPool.end();)
	{
		if (!iter->second.checkMarked())
		{
			structPool.erase(iter++);
			continue;
		}
		++iter;
	}
}

Type Type::Factory::getSimpleType(String name)
{
	auto iter = simplePool.find(name);
	if (iter == simplePool.end())
	{
		auto impl_ptr = new Type::Impl(context, name);
		impl_ptr->hashCode = name.hash();
		iter = simplePool.emplace(name, impl_ptr).first;
	}
	return iter->second;
}

TemplateType Type::Factory::getTemplateType(String name, const std::vector<Symbol>& args)
{
	TemplateType::Data data = { name, args };
	auto iter = templatePool.find(data);
	if (iter == templatePool.end())
	{
		auto impl_ptr = new TemplateType::Impl(context, name, args);
		impl_ptr->hashCode = std::hash<TemplateType::Data>()(data);
		iter = templatePool.emplace(data, impl_ptr).first;
	}
	return iter->second;
}

ArrayType Type::Factory::getArrayType(Type elementType, const ArrayDim& dim)
{
	auto name = context->getString("array");
	ArrayType::Data data = { elementType, dim };
	auto iter = arrayPool.find(data);
	if (iter == arrayPool.end())
	{
		auto impl_ptr = new ArrayType::Impl(context, name, elementType, dim);
		impl_ptr->hashCode = std::hash<ArrayType::Data>()(data);
		iter = arrayPool.emplace(data, impl_ptr).first;
	}
	return iter->second;
}

TupleType Type::Factory::getTupleType(const std::vector<Type>& elementTypes)
{
	auto name = context->getString("tuple");
	TupleType::Data data = { elementTypes };
	auto iter = tuplePool.find(data);
	if (iter == tuplePool.end())
	{
		auto impl_ptr = new TupleType::Impl(context, name, elementTypes);
		impl_ptr->hashCode = std::hash<TupleType::Data>()(data);
		iter = tuplePool.emplace(data, impl_ptr).first;
	}
	return iter->second;
}

StructType Type::Factory::getStructType(String name, const std::vector<Type>& elementTypes, const std::vector<String>& elementNames)
{
	StructType::Data data = { elementTypes, name, elementNames };
	auto iter = structPool.find(data);
	if (iter == structPool.end())
	{
		auto impl_ptr = new StructType::Impl(context, name, elementTypes, elementNames);
		impl_ptr->hashCode = std::hash<StructType::Data>()(data);
		iter = structPool.emplace(data, impl_ptr).first;
	}
	return iter->second;
}

Type Type::Factory::getLogicType() const
{
	return context->getSimpleType(context->getString("Logic"));
}

Type Type::Factory::getComplexType() const
{
	return  context->getSimpleType(context->getString("Complex"));
}

Type Type::Factory::getRealType() const
{
	return  context->getSimpleType(context->getString("Real"));
}

Type Type::Factory::getRationalType() const
{
	return  context->getSimpleType(context->getString("Rational"));
}

Type Type::Factory::getIntegerType() const
{
	return  context->getSimpleType(context->getString("Integer"));
}

Type Type::Factory::getBitType() const
{
	return  context->getSimpleType(context->getString("bit"));
}

Type Type::Factory::getByteType() const
{
	return  context->getSimpleType(context->getString("byte"));
}

TemplateType Type::Factory::getIntType(uint64_t bits) const
{
	return  context->getTemplateType(context->getString("int"),
		{ context->getInteger(bits) });
}

TemplateType Type::Factory::getFloatType(uint64_t exponent, uint64_t mantissa) const
{
	return  context->getTemplateType(context->getString("float"),
		{ context->getInteger(exponent), context->getInteger(mantissa) });
}

std::optional<size_t> StructType::getIndexByName(String name) const
{
	auto iter = std::find(getElementNameVector().begin(), getElementNameVector().end(), name);
	if (iter == getElementNameVector().end())
	{
		return std::nullopt;
	}
	return iter - getElementNameVector().begin();
}
