#define VCG_EXPORTS

#include "Context.h"

Decimal::Impl::Impl(Context* context, big_decimal data)
	: Symbol::Impl(context), data(std::move(data))
{
	hashCode = std::hash<big_decimal>()(this->data);
}

std::string Decimal::Impl::toString() const
{
	std::stringstream ss;
	ss << data;
	return ss.str();
}

Decimal::Decimal(Integer src)
{
	impl_ptr = getContext()->getDecimal(big_decimal(src.getData())).impl_ptr;
}

int Decimal::sign() const
{
	return getData().sign();
}

bool Decimal::isZero() const
{
	return getData().is_zero();
}

Decimal Decimal::operator-() const
{
	return getContext()->getDecimal(-getData());
}

Decimal Decimal::operator+(const Decimal& other) const
{
	return getContext()->getDecimal(getData() + other.getData());
}

Decimal Decimal::operator-(const Decimal& other) const
{
	return getContext()->getDecimal(getData() - other.getData());
}

Decimal Decimal::operator*(const Decimal& other) const
{
	return getContext()->getDecimal(getData() * other.getData());
}

Decimal Decimal::operator/(const Decimal& other) const
{
	return getContext()->getDecimal(getData() / other.getData());
}

int Decimal::compare(const Decimal& other) const
{
	return getData().compare(other.getData());
}

bool Decimal::operator>(const Decimal& other) const
{
	return getData() > other.getData();
}

bool Decimal::operator<(const Decimal& other) const
{
	return getData() < other.getData();
}

bool Decimal::operator>=(const Decimal& other) const
{
	return getData() >= other.getData();
}

bool Decimal::operator<=(const Decimal& other) const
{
	return getData() <= other.getData();
}

Decimal::Factory::Factory(Context* context)
	: FlyweightObject::Factory(context)
{
}

void Decimal::Factory::eraseUseless()
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

Decimal Decimal::Factory::getDecimal(const big_decimal& data)
{
	auto iter = objectPool.find(&data);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new Decimal::Impl(context, data);
		iter = objectPool.emplace(&impl_ptr->data, impl_ptr).first;
	}
	return iter->second;
}
