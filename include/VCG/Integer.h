#pragma once

/**
 * @file Integer.h
 * @brief 
 */

#include "Symbol.h"

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/multiprecision/cpp_dec_float.hpp>

typedef boost::multiprecision::cpp_int big_int;

/// 
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

	/// 
	const big_int& getData() const { return getImpl().data; }

	/// 
	VCG_API int sign() const;

	/// 0
	VCG_API bool isZero() const;

	/// 
	Integer operator+() const { return *this; }

	/// 
	VCG_API Integer operator-() const;

	/// 
	VCG_API Integer operator+(const Integer& other) const;

	/// 
	VCG_API Integer& operator+=(const Integer& other) { return *this = *this + other; }

	/// 
	VCG_API Integer operator-(const Integer& other) const;

	/// 
	VCG_API Integer& operator-=(const Integer& other) { return *this = *this - other; }

	/// 
	VCG_API Integer operator*(const Integer& other) const;

	/// 
	VCG_API Integer& operator*=(const Integer& other) { return *this = *this * other; }

	/// 
	VCG_API Integer operator/(const Integer& other) const;

	/// 
	VCG_API Integer& operator/=(const Integer& other) { return *this = *this / other; }

	/// 
	VCG_API Integer operator%(const Integer& other) const;

	/// 
	VCG_API Integer& operator%=(const Integer& other) { return *this = *this % other; }

	/// 
	VCG_API int compare(const Integer& other) const;

	/// 
	VCG_API bool operator>(const Integer& other) const;

	/// 
	VCG_API bool operator<(const Integer& other) const;

	/// 
	VCG_API bool operator>=(const Integer& other) const;

	/// 
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
	/// 
	VCG_API Integer getInteger(const big_int& data);

	/// 0
	Integer getZero() const { return zeroConstant; }

private:
	std::unordered_map<const big_int*, Integer, PointerHasher<big_int>, PointerEqualTo<big_int>> objectPool;
	Integer zeroConstant;
	FlyweightObjectGuard zeroConstantGuard;
};
