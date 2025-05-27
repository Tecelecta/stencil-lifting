#pragma once

/**
 * @file String.h
 * @brief 使用享元模式的只读字符串类
 */

#include "Symbol.h"

#include <unordered_map>

/**
	* @brief 使用享元模式的只读字符串类
	* @details
	* 在内存中使用 String::Impl 类表示，然后使用该类的作为接口。
	* 使用值传递，相比std::string更高效，相比std::string_view更安全
	* 通常用于名称字段，对于字符串类型的常量，也使用此类表示字面量的值
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

	/// 获取C++标准库风格字符串引用
	const std::string& str() const { return getImpl().data; }

	/// 获取C++标准库字符串视图
	const std::string_view view() const { return getImpl().data; }

	/// 获取C风格字符串指针
	const char* c_str() const { return str().c_str(); }

	/// 判断字符串是否为空
	bool isEmpty() const { return str().empty(); }

	/// 判断字符串是否为有效（非null且非空）
	bool isValid() const { return !(impl_ptr == nullptr || isEmpty()); }

	/// 判断是否与C字符串相等
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
	/// 获取只读字符串对象
	VCG_API String getString(std::string_view data);

	/// 获取只读空字符串对象
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
