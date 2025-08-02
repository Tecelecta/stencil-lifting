#pragma once

/**
 * @file BitVector.h
 * @brief 
 */

#define VCG_EXPORTS

#include "Symbol.h"

#include <boost/dynamic_bitset.hpp>

typedef boost::dynamic_bitset<uint64_t> bit_vector;

/**
	* @brief 
	* @details
	* 
	*/
class BitVector : public Symbol
{
protected:
	class Impl : public Symbol::Impl
	{
	public:
		Impl(Context* context, bit_vector data);

		VCG_API std::string toString() const override;

	public:
		bit_vector data;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	BitVector(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// 
	const bit_vector& getData() const { return getImpl().data; }

	/// 
	bool getBit(size_t i) const { return getData().at(i); }

	/// 
	size_t size() const { return getData().size(); }

	/// 
	bool empty() const { return getData().empty(); }

	/// 64bit
	size_t getNumBlocks() const { return getData().num_blocks(); }

	/// 
	VCG_API BitVector operator&(const BitVector& other) const;

	/// 
	BitVector& operator&=(const BitVector& other) { return *this = *this & other; }

	/// 
	VCG_API BitVector operator|(const BitVector& other) const;

	/// 
	BitVector& operator|=(const BitVector& other) { return *this = *this | other; }

	/// 
	VCG_API BitVector operator^(const BitVector& other) const;

	/// 
	BitVector& operator^=(const BitVector& other) { return *this = *this ^ other; }

	/// 
	VCG_API BitVector operator~() const;

	/// 1
	VCG_API size_t count() const;

	/// 1
	VCG_API bool all() const;

	/// 1
	VCG_API bool any() const;

	/// 1
	VCG_API bool none() const;

	/// 0
	VCG_API size_t clz() const;

	/// 
	VCG_API BitVector trim();

public:
	class Factory;
};

template<>
inline BitVector Symbol::cast<BitVector>() const
{
	return dynamic_cast<BitVector::Impl*>(impl_ptr);
}

STD_HASH(BitVector)

class BitVector::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context) : FlyweightObject::Factory(context) {}
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/// 
	VCG_API BitVector getBitVector(const bit_vector& data);

	/// 64bit
	VCG_API BitVector getBitVector(const std::vector<uint64_t>& data, size_t num_bits);

private:
	std::unordered_map<const bit_vector*, BitVector, PointerHasher<bit_vector>, PointerEqualTo<bit_vector>> objectPool;
};
