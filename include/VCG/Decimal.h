#pragma once

/**
 * @file Decimal.h
 * @brief 
 */

#include "Integer.h"

#include <boost/multiprecision/cpp_dec_float.hpp>

typedef boost::multiprecision::cpp_dec_float_100 big_decimal;

/// 
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

	/// 
	const big_decimal& getData() const { return getImpl().data; }

	/// 
	VCG_API int sign() const;

	/// 0
	VCG_API bool isZero() const;

	/// 
	Decimal operator+() const { return *this; }

	/// 
	VCG_API Decimal operator-() const;

	/// 
	VCG_API Decimal operator+(const Decimal& other) const;

	/// 
	VCG_API Decimal& operator+=(const Decimal& other) { return *this = *this + other; }

	/// 
	VCG_API Decimal operator-(const Decimal& other) const;

	/// 
	VCG_API Decimal& operator-=(const Decimal& other) { return *this = *this - other; }

	/// 
	VCG_API Decimal operator*(const Decimal& other) const;

	/// 
	VCG_API Decimal& operator*=(const Decimal& other) { return *this = *this * other; }

	/// 
	VCG_API Decimal operator/(const Decimal& other) const;

	/// 
	VCG_API Decimal& operator/=(const Decimal& other) { return *this = *this / other; }

	/// 
	VCG_API int compare(const Decimal& other) const;

	/// 
	VCG_API bool operator>(const Decimal& other) const;

	/// 
	VCG_API bool operator<(const Decimal& other) const;

	/// 
	VCG_API bool operator>=(const Decimal& other) const;

	/// 
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
	/// 
	VCG_API Decimal getDecimal(const big_decimal& data);

private:
	std::unordered_map<const big_decimal*, Decimal, PointerHasher<big_decimal>, PointerEqualTo<big_decimal>> objectPool;
};
