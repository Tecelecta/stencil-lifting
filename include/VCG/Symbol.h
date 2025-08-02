#pragma once

/**
 * @file Symbol.h
 * @brief 
 */

#include "FlyweightObject.h"

#include <string>

/// 
class Symbol : public FlyweightObject
{
protected:
	class Impl : public FlyweightObject::Impl
	{
	public:
		explicit Impl(Context* context) : FlyweightObject::Impl(context) {}

		virtual std::string toString() const = 0;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

public:
	Symbol(Impl* impl_ptr = nullptr) : FlyweightObject(impl_ptr) {}

	/// 
	template<typename T>
	T cast() const;

	/// 
	std::string toString() const { return getImpl().toString(); }
};

STD_HASH(Symbol)
