#define VCG_EXPORTS

#include "Context.h"

#include <sstream>

Integer::Impl::Impl(Context* context, big_int data)
	: Symbol::Impl(context), data(std::move(data))
{
	hashCode = std::hash<big_int>()(this->data);
}

std::string Integer::Impl::toString() const
{
	std::stringstream ss;
	ss << data;
	return ss.str();
}

int Integer::sign() const
{
	return getData().sign();
}

bool Integer::isZero() const
{
	return getData().is_zero();
}

Integer Integer::operator-() const
{
	return getContext()->getInteger(-getData());
}

Integer Integer::operator+(const Integer& other) const
{
	return getContext()->getInteger(getData() + other.getData());
}

Integer Integer::operator-(const Integer& other) const
{
	return getContext()->getInteger(getData() - other.getData());
}

Integer Integer::operator*(const Integer& other) const
{
	return getContext()->getInteger(getData() * other.getData());
}

Integer Integer::operator/(const Integer& other) const
{
	return getContext()->getInteger(getData() / other.getData());
}

Integer Integer::operator%(const Integer& other) const
{
	return getContext()->getInteger(getData() % other.getData());
}

int Integer::compare(const Integer& other) const
{
	return getData().compare(other.getData());
}

bool Integer::operator>(const Integer& other) const
{
	return getData() > other.getData();
}

bool Integer::operator<(const Integer& other) const
{
	return getData() < other.getData();
}

bool Integer::operator>=(const Integer& other) const
{
	return getData() >= other.getData();
}

bool Integer::operator<=(const Integer& other) const
{
	return getData() <= other.getData();
}

Integer::Factory::Factory(Context* context)
	: FlyweightObject::Factory(context)
{
	zeroConstant = getInteger(0);
	zeroConstantGuard = FlyweightObjectGuard(zeroConstant);
}

void Integer::Factory::eraseUseless()
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

Integer Integer::Factory::getInteger(const big_int& data)
{
	auto iter = objectPool.find(&data);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new Integer::Impl(context, data);
		iter = objectPool.emplace(&impl_ptr->data, impl_ptr).first;
	}
	return iter->second;
}
