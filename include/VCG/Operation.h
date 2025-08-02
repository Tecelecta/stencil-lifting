#pragma once

/**
 * @file Operation.h
 * @brief 
 */

#include "Type.h"

/**
	* @brief VCG
	* @details
	*  Operation::Impl 
	*/
class Operation : public Symbol
{
public:
	/// DSL
	struct Overload
	{
		String name; //!< 
		std::vector<Type> parameterTypes; //!< 

		DECL_HASH_EQ_NE_API(Overload)
	};

	/// IR
	struct Data
	{
		String name; //!< 
		std::vector<Type> parameterTypes; //!< 
		Type returnType; //!< 

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

	/// 
	String getName() const { return getImpl().name; }

	/// 
	const std::vector<Type>& getParameterTypeVector() const { return getImpl().parameterTypes; }

	/// 
	Type getParameterType(size_t i) const { return getImpl().parameterTypes[i]; }

	/// 
	size_t getParameterNum() const { return getImpl().parameterTypes.size(); }

	/// 
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
		* @brief 
		* @param data 
		*/
	VCG_API Operation getOperation(Operation::Data data);

	/**
		* @brief 
		* @param name_and_parameterTypes 
		* @param returnType 
		*/
	VCG_API Operation getOperation(Operation::Overload name_and_parameterTypes, Type returnType);

	/**
		* @brief 
		* @param name 
		* @param parameterTypes 
		* @param returnType 
		*/
	VCG_API Operation getOperation(String name, const std::vector<Type>& parameterTypes, Type returnType);
		
	/**
		* @brief 
		* @param type 
		*/
	VCG_API Operation getCopyOperation(Type type);
		
	/**
		* @brief 
		* @param type 
		*/
	VCG_API Operation getBinarySelectOperation(Type type);
		
	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getArrayGetOperation(ArrayType arrayType);

	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getArraySetOperation(ArrayType arrayType);

	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getArrayLowerBoundOperation(ArrayType arrayType);
		
	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getArrayUpperBoundOperation(ArrayType arrayType);
		
	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getTupleGetOperation(TupleType tupleType, size_t index);
		
	/**
		* @brief 
		* @param arrayType 
		*/
	VCG_API Operation getTupleSetOperation(TupleType tupleType, size_t index);

private:
	std::unordered_map<Operation::Data, Operation> objectPool;
};
