#pragma once

/**
 * @file Symbol.h
 * @brief 使用享元模式定义符号计算所使用的数据结构基类
 */

#include "FlyweightObject.h"

#include <string>

/// 符号（抽象类），是符号计算所使用的基类
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

	/// 动态转换到某个子类，在子类中通过偏特化定义
	template<typename T>
	T cast() const;

	/// 获取字符串，用于输出与调试
	std::string toString() const { return getImpl().toString(); }
};

STD_HASH(Symbol)
