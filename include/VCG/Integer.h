#pragma once

/**
 * @file Integer.h
 * @brief 定义高精度整数的数据结构
 */

#include "Symbol.h"

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/multiprecision/cpp_dec_float.hpp>

typedef boost::multiprecision::cpp_int big_int;

/// 高精度整数
class Integer : public Symbol
{
protected:
	class Impl : public Symbol::Impl
	{
	public:
		Impl(Context* context, big_int data);

		std::string toString() const override;

	public:
		big_int data;
	};

	const Impl& getImpl() const { return *reinterpret_cast<const Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Integer(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// 获取数据
	const big_int& getData() const { return getImpl().data; }

	/// 获取整数的符号
	VCG_API int sign() const;

	/// 是否为0
	VCG_API bool isZero() const;

	/// 正号，返回自身
	Integer operator+() const { return *this; }

	/// 取相反数，并创建新对象
	VCG_API Integer operator-() const;

	/// 加法，并创建新对象
	VCG_API Integer operator+(const Integer& other) const;

	/// 加法，并赋值给自己
	VCG_API Integer& operator+=(const Integer& other) { return *this = *this + other; }

	/// 减法，并创建新对象
	VCG_API Integer operator-(const Integer& other) const;

	/// 减法，并赋值给自己
	VCG_API Integer& operator-=(const Integer& other) { return *this = *this - other; }

	/// 乘法，并创建新对象
	VCG_API Integer operator*(const Integer& other) const;

	/// 乘法，并赋值给自己
	VCG_API Integer& operator*=(const Integer& other) { return *this = *this * other; }

	/// 整除，并创建新对象
	VCG_API Integer operator/(const Integer& other) const;

	/// 整除，并赋值给自己
	VCG_API Integer& operator/=(const Integer& other) { return *this = *this / other; }

	/// 求余，并创建新对象
	VCG_API Integer operator%(const Integer& other) const;

	/// 求余，并赋值给自己
	VCG_API Integer& operator%=(const Integer& other) { return *this = *this % other; }

	/// 比较大小
	VCG_API int compare(const Integer& other) const;

	/// 判断是否大于
	VCG_API bool operator>(const Integer& other) const;

	/// 判断是否小于
	VCG_API bool operator<(const Integer& other) const;

	/// 判断是否大于等于
	VCG_API bool operator>=(const Integer& other) const;

	/// 判断是否小于等于
	VCG_API bool operator<= (const Integer& other) const;

public:
	class Factory;
};

template<>
inline Integer Symbol::cast<Integer>() const
{
	return dynamic_cast<Integer::Impl*>(impl_ptr);
}

STD_HASH(Integer)

class Integer::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context);
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/// 获取高精度整数对象
	VCG_API Integer getInteger(const big_int& data);

	/// 获取值为0的高精度整数对象
	Integer getZero() const { return zeroConstant; }

private:
	std::unordered_map<const big_int*, Integer, PointerHasher<big_int>, PointerEqualTo<big_int>> objectPool;
	Integer zeroConstant;
	FlyweightObjectGuard zeroConstantGuard;
};
