#pragma once

/**
 * @file Enum.h
 * @brief 将枚举类型包装成Symbol的子类
 */

#include "Symbol.h"

#include <typeinfo>
#include <unordered_map>

using std::type_info;

/// 表示一个枚举类型的符号
class Enum : public Symbol
{
public:
	/// 枚举类型和枚举值
	struct Data
	{
		const type_info* type = nullptr;
		size_t value = 0;

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public Symbol::Impl, public Data
	{
	public:
		Impl(Context* context, const type_info* type, size_t value)
			: Symbol::Impl(context), Data{ type, value } { hashCode = type->hash_code() + value; }

		std::string toString() const override { return std::to_string(value); }
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Enum(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	const type_info* getType() const { return getImpl().type; }

	template<typename _enum>
	_enum getValue() const
	{
		if (typeid(_enum) == *getImpl().type)
		{
			return static_cast<_enum>(getImpl().value);
		}
		throw std::runtime_error("Enum type mismatch!");
	}

public:
	class Factory;
};

template<>
inline Enum Symbol::cast<Enum>() const
{
	return dynamic_cast<Enum::Impl*>(impl_ptr);
}

enum class LogicEnum
{
	FALSE, TRUE
};

class Logic : public Enum
{
public:
	Logic(Impl* impl_ptr = nullptr) : Enum(impl_ptr) {}

	bool isTrue() const { return getValue<LogicEnum>() != LogicEnum::FALSE; }

	/// 逻辑与，并创建新对象
	VCG_API Logic operator&&(const Logic& other) const;

	/// 逻辑或，并创建新对象
	VCG_API Logic operator||(const Logic& other) const;

	/// 逻辑非，并创建新对象
	VCG_API Logic operator!() const;
};

template<>
inline Logic Symbol::cast<Logic>() const
{
	auto _enum = cast<Enum>();
	return *_enum.getType() == typeid(LogicEnum) ? Logic(&_enum.getImpl()) : Logic(nullptr);
}

STD_HASH(Enum::Data)
STD_HASH(Enum)
STD_HASH(Logic)

class Enum::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context);
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

	/// 通过枚举类型和枚举值获取枚举对象
	VCG_API Enum getEnum(const type_info* type, size_t value);

public:
	/// 获取枚举对象
	template<typename _enum>
	Enum getEnum(_enum value) { return getEnum(&typeid(_enum), static_cast<size_t>(value)); }

	/// 获取逻辑类型对象
	Logic getLogic(LogicEnum value) { return getEnum(&typeid(LogicEnum), static_cast<size_t>(value)).cast<Logic>(); }

	/// 获取值为true的逻辑类型对象
	Logic getTrue() const { return trueConstant; }

	/// 获取值为false的逻辑类型对象
	Logic getFalse() const { return falseConstant; }

	/// 获取逻辑类型对象
	Logic getLogic(bool value) const { return value ? getTrue() : getFalse(); }

private:
	std::unordered_map<Enum::Data, Enum> objectPool;
	Logic trueConstant;
	Logic falseConstant;
	FlyweightObjectGuard trueConstantGuard;
	FlyweightObjectGuard falseConstantGuard;
};
