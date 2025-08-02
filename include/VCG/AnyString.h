#pragma once

/**
 * @file String.h
 * @brief 
 */

#include "Symbol.h"

#include <unordered_map>

/**
	* @brief 
	* @details
	*  String::Impl 
	* std::stringstd::string_view
	* 
	*/
class String : public Symbol
{
protected:
	class Impl : public Symbol::Impl
	{
	public:
		Impl(Context* context, std::string_view data);

		std::string toString() const override { return data; }

	public:
		std::string data;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	String(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// C++
	const std::string& str() const { return getImpl().data; }

	/// C++
	const std::string_view view() const { return getImpl().data; }

	/// C
	const char* c_str() const { return str().c_str(); }

	/// 
	bool isEmpty() const { return str().empty(); }

	/// null
	bool isValid() const { return !(impl_ptr == nullptr || isEmpty()); }

	/// C
	bool equals(std::string_view other) const { return impl_ptr != nullptr && view() == other; }

public:
	class Factory;
};

template<>
inline String Symbol::cast<String>() const
{
	return dynamic_cast<String::Impl*>(impl_ptr);
}

STD_HASH(String)

class String::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context) : FlyweightObject::Factory(context) {}
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/// 
	VCG_API String getString(std::string_view data);

	/// 
	String getEmptyString() { return getString(std::string_view()); }

private:
	std::unordered_map<std::string_view, String> objectPool;
};

#include <iostream>

inline std::ostream& operator<<(std::ostream& out, String s)
{
	if (s.isValid())
	{
		out << s.str();
	}
	return out;
}
