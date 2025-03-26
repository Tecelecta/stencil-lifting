#pragma once

/**
 * @file BitVector.h
 * @brief 定义位向量的数据结构
 */

#define VCG_EXPORTS

#include "Symbol.h"

#include <boost/dynamic_bitset.hpp>

typedef boost::dynamic_bitset<uint64_t> bit_vector;

/**
	* @brief 由可变数量个的比特按顺序构成位向量
	* @details
	* 位向量本身不带有对每个位的解释，可以存储各种基本类型和由基本类型复合而成的类型，与类型系统配合可以表示常量字面量。
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

	/// 获取数据
	const bit_vector& getData() const { return getImpl().data; }

	/// 根据索引获取某个位
	bool getBit(size_t i) const { return getData().at(i); }

	/// 获取位向量长度
	size_t size() const { return getData().size(); }

	/// 判断位向量是否为空
	bool empty() const { return getData().empty(); }

	/// 获取存储的64bit内存块的数量
	size_t getNumBlocks() const { return getData().num_blocks(); }

	/// 按位与，并创建新对象
	VCG_API BitVector operator&(const BitVector& other) const;

	/// 按位与，并赋值给自身
	BitVector& operator&=(const BitVector& other) { return *this = *this & other; }

	/// 按位或，并创建新对象
	VCG_API BitVector operator|(const BitVector& other) const;

	/// 按位或，并赋值给自身
	BitVector& operator|=(const BitVector& other) { return *this = *this | other; }

	/// 按位异或，并创建新对象
	VCG_API BitVector operator^(const BitVector& other) const;

	/// 按位异或，并赋值给自身
	BitVector& operator^=(const BitVector& other) { return *this = *this ^ other; }

	/// 按位取反，并创建新对象
	VCG_API BitVector operator~() const;

	/// 计算整个位向量中1的数量
	VCG_API size_t count() const;

	/// 判断是否整个位向量都是1
	VCG_API bool all() const;

	/// 判断位向量中是否存在1
	VCG_API bool any() const;

	/// 判断位向量中是否没有1
	VCG_API bool none() const;

	/// 计算前导0数量
	VCG_API size_t clz() const;

	/// 清除最高位，并创建新对象
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
	/// 获取位向量对象
	VCG_API BitVector getBitVector(const bit_vector& data);

	/// 以64bit内存块构造位向量对象
	VCG_API BitVector getBitVector(const std::vector<uint64_t>& data, size_t num_bits);

private:
	std::unordered_map<const bit_vector*, BitVector, PointerHasher<bit_vector>, PointerEqualTo<bit_vector>> objectPool;
};
