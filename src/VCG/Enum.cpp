#define VCG_EXPORTS

#include "Context.h"

#include <boost/pfr.hpp>

BOOST_PFR_HASH_EQ_NE_API(Enum::Data)

Logic Logic::operator&&(const Logic& other) const
{
	return getContext()->getLogic(this->isTrue() && other.isTrue());
}

Logic Logic::operator||(const Logic& other) const
{
	return getContext()->getLogic(this->isTrue() || other.isTrue());
}

Logic Logic::operator!() const
{
	return getContext()->getLogic(!this->isTrue());
}

Enum::Factory::Factory(Context* context)
	: FlyweightObject::Factory(context)
{
	trueConstant = getLogic(LogicEnum::TRUE);
	falseConstant = getLogic(LogicEnum::FALSE);
	trueConstantGuard = FlyweightObjectGuard(trueConstant);
	falseConstantGuard = FlyweightObjectGuard(falseConstant);
}

void Enum::Factory::eraseUseless()
{
	for (auto iter = objectPool.begin(); iter != objectPool.end();)
	{
		if (!iter->second.checkMarked())
		{
			objectPool.erase(iter++);
			continue;
		}
		iter++;
	}
}

Enum Enum::Factory::getEnum(const type_info* type, size_t value)
{
	auto iter = objectPool.find({ type, value });
	if (iter == objectPool.end())
	{
		auto impl_ptr = new Enum::Impl(context, type, value);
		iter = objectPool.emplace(Enum::Data{ type, value }, impl_ptr).first;
	}
	return iter->second;
}
