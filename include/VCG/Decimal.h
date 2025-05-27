#pragma once

/**
 * @file Decimal.h
 * @brief 定义高精度十进制小数的数据结构
 */

#include "Integer.h"

#include <boost/multiprecision/cpp_dec_float.hpp>

typedef boost::multiprecision::cpp_dec_float_100 big_decimal;

/// 高精度十进制小数
class Decimal : public Symbol
{
protected:
	class Impl : public Symbol::Impl
	{
	public:
		Impl(Context* context, big_decimal data);

		std::string toString() const override;

	public:
		big_decimal data;
	};

	const Impl& getImpl() const { return *reinterpret_cast<const Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Decimal(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	VCG_API Decimal(Integer src);

	/// 获取数据
	const big_decimal& getData() const { return getImpl().data; }

	/// 获取十进制小数的符号
	VCG_API int sign() const;

	/// 是否为0
	VCG_API bool isZero() const;

	/// 正号，返回自身
	Decimal operator+() const { return *this; }

	/// 取相反数，并创建新对象
	VCG_API Decimal operator-() const;

	/// 加法，并创建新对象
	VCG_API Decimal operator+(const Decimal& other) const;

	/// 加法，并赋值给自己
	VCG_API Decimal& operator+=(const Decimal& other) { return *this = *this + other; }

	/// 减法，并创建新对象
	VCG_API Decimal operator-(const Decimal& other) const;

	/// 减法，并赋值给自己
	VCG_API Decimal& operator-=(const Decimal& other) { return *this = *this - other; }

	/// 乘法，并创建新对象
	VCG_API Decimal operator*(const Decimal& other) const;

	/// 乘法，并赋值给自己
	VCG_API Decimal& operator*=(const Decimal& other) { return *this = *this * other; }

	/// 除法，并创建新对象
	VCG_API Decimal operator/(const Decimal& other) const;

	/// 除法，并赋值给自己
	VCG_API Decimal& operator/=(const Decimal& other) { return *this = *this / other; }

	/// 比较大小
	VCG_API int compare(const Decimal& other) const;

	/// 判断是否大于
	VCG_API bool operator>(const Decimal& other) const;

	/// 判断是否小于
	VCG_API bool operator<(const Decimal& other) const;

	/// 判断是否大于等于
	VCG_API bool operator>=(const Decimal& other) const;

	/// 判断是否小于等于
	VCG_API bool operator<= (const Decimal& other) const;

public:
	class Factory;
};

template<>
inline Decimal Symbol::cast<Decimal>() const
{
	return dynamic_cast<Decimal::Impl*>(impl_ptr);
}

STD_HASH(Decimal)

class Decimal::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context);
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/// 获取高精度十进制小数对象
	VCG_API Decimal getDecimal(const big_decimal& data);

private:
	std::unordered_map<const big_decimal*, Decimal, PointerHasher<big_decimal>, PointerEqualTo<big_decimal>> objectPool;
};
