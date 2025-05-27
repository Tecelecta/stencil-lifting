#include "Context.h"

#include <sstream>

BitVector::Impl::Impl(Context* context, bit_vector data)
	: Symbol::Impl(context), data(std::move(data))
{
	this->data.shrink_to_fit();
	hashCode = std::hash<bit_vector>()(this->data);
}

std::string BitVector::Impl::toString() const
{
	std::stringstream ss;
	ss << data;
	return ss.str();
}

BitVector BitVector::operator&(const BitVector& other) const
{
	return getContext()->getBitVector(getData() & other.getData());
}

BitVector BitVector::operator|(const BitVector& other) const
{
	return getContext()->getBitVector(getData() | other.getData());
}

BitVector BitVector::operator^(const BitVector& other) const
{
	return getContext()->getBitVector(getData() ^ other.getData());
}

BitVector BitVector::operator~()const
{
	return getContext()->getBitVector(~getData());
}

size_t BitVector::count() const
{
	return getData().count();
}

bool BitVector::all() const
{
	return getData().all();
}

bool BitVector::any() const
{
	return getData().all();
}

bool BitVector::none() const
{
	return getData().none();
}

size_t BitVector::clz() const
{
	const auto& data = getData();
	if (data.empty())
	{
		return 0;
	}
	size_t i = size();
	size_t result = 0;
	while (--i > 0)
	{
		result += getBit(i) ? 1 : 0;
	}
	return result += getBit(i) ? 1 : 0;
}

BitVector BitVector::trim()
{
	size_t zero_num = clz();
	if (zero_num == 0)
	{
		return *this;
	}
	bit_vector data = getData();
	data.resize(data.size() - zero_num);
	return getContext()->getBitVector(data);
}

void BitVector::Factory::eraseUseless()
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

BitVector BitVector::Factory::getBitVector(const bit_vector& data)
{
	auto iter = objectPool.find(&data);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new BitVector::Impl(context, data);
		iter = objectPool.emplace(&impl_ptr->data, impl_ptr).first;
	}
	return iter->second;
}

BitVector BitVector::Factory::getBitVector(const std::vector<uint64_t>& data, size_t num_bits)
{
	bit_vector bv;
	bv.append(data.begin(), data.end());
	bv.resize(num_bits);
	auto iter = objectPool.find(&bv);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new BitVector::Impl(context, std::move(bv));
		iter = objectPool.emplace(&impl_ptr->data, impl_ptr).first;
	}
	return iter->second;
}
