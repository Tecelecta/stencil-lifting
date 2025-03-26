#pragma once

/**
 * @file Operation.h
 * @brief 定义操作码的数据结构
 */

#include "Type.h"

/**
	* @brief VCG中的操作码，用于支持同名操作的重载
	* @details
	* 每个操作码由名称、参数类型和返回值类型组成，在内存中使用 Operation::Impl 类表示，然后使用该类的作为接口。
	*/
class Operation : public Symbol
{
public:
	/// DSL源码按名称和参数重载
	struct Overload
	{
		String name; //!< 操作名称
		std::vector<Type> parameterTypes; //!< 参数类型

		DECL_HASH_EQ_NE_API(Overload)
	};

	/// 名称和参数，再加上返回值，构成操作码在IR中的重载
	struct Data
	{
		String name; //!< 操作名称
		std::vector<Type> parameterTypes; //!< 参数类型
		Type returnType; //!< 返回值类型

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public Symbol::Impl, public Data
	{
	public:
		explicit Impl(Context* context, Data data)
			: Symbol::Impl(context), Data(std::move(data)) { hashCode = Data::hash(); }

		std::string toString() const override { return name.str(); }

	protected:
		void markReference() override;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Operation(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// 获取操作名称
	String getName() const { return getImpl().name; }

	/// 获取所有参数类型
	const std::vector<Type>& getParameterTypeVector() const { return getImpl().parameterTypes; }

	/// 根据索引获取某一个参数类型
	Type getParameterType(size_t i) const { return getImpl().parameterTypes[i]; }

	/// 获取参数数量
	size_t getParameterNum() const { return getImpl().parameterTypes.size(); }

	/// 获取返回值类型
	Type getReturnType() const { return getImpl().returnType; }

public:
	class Factory;
	friend Factory;
};

template<>
inline Operation Symbol::cast<Operation>() const
{
	return dynamic_cast<Operation::Impl*>(impl_ptr);
}

STD_HASH(Operation::Overload)
STD_HASH(Operation::Data)
STD_HASH(Operation)

class Operation::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context) : FlyweightObject::Factory(context) {}
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/**
		* @brief 根据数据获取操作码对象
		* @param data 操作的完整声明数据
		*/
	VCG_API Operation getOperation(Operation::Data data);

	/**
		* @brief 根据数据获取操作码对象
		* @param name_and_parameterTypes 操作名称和参数类型
		* @param returnType 返回值类型
		*/
	VCG_API Operation getOperation(Operation::Overload name_and_parameterTypes, Type returnType);

	/**
		* @brief 根据数据获取操作码对象
		* @param name 操作名称
		* @param parameterTypes 参数类型
		* @param returnType 返回值类型
		*/
	VCG_API Operation getOperation(String name, const std::vector<Type>& parameterTypes, Type returnType);
		
	/**
		* @brief 获取表示拷贝的操作码对象
		* @param type 操作数类型
		*/
	VCG_API Operation getCopyOperation(Type type);
		
	/**
		* @brief 获取表示二元条件选择的操作码对象
		* @param type 操作数类型
		*/
	VCG_API Operation getBinarySelectOperation(Type type);
		
	/**
		* @brief 获取数组读操作的操作码对象
		* @param arrayType 数组类型
		*/
	VCG_API Operation getArrayGetOperation(ArrayType arrayType);

	/**
		* @brief 获取数组写操作的操作码对象
		* @param arrayType 数组类型
		*/
	VCG_API Operation getArraySetOperation(ArrayType arrayType);

	/**
		* @brief 获取数组下界的操作码对象
		* @param arrayType 数组类型
		*/
	VCG_API Operation getArrayLowerBoundOperation(ArrayType arrayType);
		
	/**
		* @brief 获取数组上界的操作码对象
		* @param arrayType 数组类型
		*/
	VCG_API Operation getArrayUpperBoundOperation(ArrayType arrayType);
		
	/**
		* @brief 获取元组读操作的操作码对象
		* @param arrayType 元组类型
		*/
	VCG_API Operation getTupleGetOperation(TupleType tupleType, size_t index);
		
	/**
		* @brief 获取元组写操作的操作码对象
		* @param arrayType 元组类型
		*/
	VCG_API Operation getTupleSetOperation(TupleType tupleType, size_t index);

private:
	std::unordered_map<Operation::Data, Operation> objectPool;
};
