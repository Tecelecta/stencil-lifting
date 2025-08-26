#pragma once

/**
 * @file Type.h
 * @brief 
 */

#include "AnyString.h"
#include "Enum.h"
#include "BitVector.h"
#include "Integer.h"
#include "Decimal.h"

#include <optional>

/**
	* @brief VCG
	* @details
	*  Type::Impl 
	*/
class Type : public Symbol
{
protected:
	class Impl : public Symbol::Impl
	{
	public:
		Impl(Context* context, String name)
			: Symbol::Impl(context), name(name) {}

		virtual bool isCovariant(Type other) const { return false; }

	protected:
		void markReference() override;

		std::string toString() const override { return name.str(); }

	public:
		String name; //!< 
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Type(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// 
	String getName() const { return getImpl().name; }

	/// VCG
	VCG_API bool inheritsFrom(Type other) const;

	/// VCG
	bool isCovariant(Type other) const { return getImpl().isCovariant(other); }

	/**
		* @brief VCG
		* @details
		* 
		*/
	bool canBeArgument(Type other) const { return *this == other || inheritsFrom(other) || isCovariant(other); }

	/**
		* @brief 
		* @return 
		* @return 0
		*/
	VCG_API size_t getNumDims() const;

	/**
		* @brief 
		* @return 
		* @return null
		* @return 1
		*/
	VCG_API Integer getNumElements() const;

public:
	class Factory;

	template<typename Ret, typename... Args>
	class Visitor;
};

template<>
inline Type Symbol::cast<Type>() const
{
	return dynamic_cast<Type::Impl*>(impl_ptr);
}

/**
	* @brief 
	* @details
	* 
	*/
class TemplateType : public Type
{
public:
	/// unordered_mapkey
	struct Data
	{
		String name; //!< 
		std::vector<Symbol> args; //!< 

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public Type::Impl
	{
	public:
		Impl(Context* context, String name, std::vector<Symbol> args)
			: Type::Impl(context, name), argumentVector(std::move(args)) {}

	protected:
		void markReference() override;

	public:
		std::vector<Symbol> argumentVector; //!< 
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	TemplateType(Impl* impl_ptr = nullptr) : Type(impl_ptr) {}

	/// 
	const std::vector<Symbol>& getArgumentVector() const { return getImpl().argumentVector; }

	/// 
	Symbol getArgument(size_t i) const { return getImpl().argumentVector[i]; }

	/// 
	size_t getArgumentVectorSize() const { return getImpl().argumentVector.size(); }

	friend Type::Factory;
};

template<>
inline TemplateType Symbol::cast<TemplateType>() const
{
	return dynamic_cast<TemplateType::Impl*>(impl_ptr);
}

/**
	* @brief 
	* @details
	* 
	* 
	* 
	*/
class CompositeType : public Type
{
protected:
	class Impl : public Type::Impl
	{
	public:
		Impl(Context* context, String name)
			: Type::Impl(context, name) {}
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	CompositeType(Impl* impl_ptr = nullptr) : Type(impl_ptr) {}
};

template<>
inline CompositeType Symbol::cast<CompositeType>() const
{
	return dynamic_cast<CompositeType::Impl*>(impl_ptr);
}

/// 
struct ArrayDim
{
	/// 
	struct Range
	{
		Integer lower;
		Integer upper;

		DECL_HASH_EQ_NE_API(Range)
	};

	/**
		* @brief 
		* @param numDims 
		*/
	VCG_API ArrayDim(size_t numDims);

	/**
		* @brief C0
		* @param data null
		*/
	VCG_API ArrayDim(const std::vector<Integer>& data); // 

	/**
		* @brief Fortran
		* @param data (first)(second)null
		*/
	VCG_API ArrayDim(std::vector<Range> data);

	static constexpr size_t MAX_NUM_DIMS = 100;

	/// 
	size_t getNumDims() const { return data.size(); }

	/// 
	Range& operator[](size_t i) { return data[i]; }

	/// 
	const Range& operator[](size_t i) const { return data[i]; }

	/// SDSL
	std::string toString() const;

	/// 
	std::vector<Range> data;

	DECL_HASH_EQ_NE_API(ArrayDim)
};

/**
	* @brief 
	* @details
	* 
	* 
	*/
class ArrayType : public CompositeType
{
public:
	/// unordered_mapkey
	struct Data
	{
		Type elementType; //!< 
		ArrayDim dim; //!< 

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public CompositeType::Impl, public Data
	{
	public:
		Impl(Context* context, String name, Type elementType, ArrayDim dim)
			: CompositeType::Impl(context, name), Data{ elementType, std::move(dim) } {}

		bool isCovariant(Type other) const override;

		std::string toString() const override;

	protected:
		void markReference() override;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	ArrayType(Impl* impl_ptr = nullptr) : CompositeType(impl_ptr) {}

	/// 
	Type getElementType() const { return getImpl().elementType; }

	/// 
	const ArrayDim& getDimObject() const { return getImpl().dim; }

	/// 
	const ArrayDim::Range& getDim(size_t i) const { return getDimObject()[i]; }

	/// 
	size_t getNumDims() const { return getDimObject().getNumDims(); }
};

template<>
inline ArrayType Symbol::cast() const
{
	return dynamic_cast<ArrayType::Impl*>(impl_ptr);
}

/**
	* @brief 
	* @details
	* 
	* 
	*/
class TupleType : public CompositeType
{
public:
	/// unordered_mapkey
	struct Data
	{
		std::vector<Type> elementTypes; //!< 

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public CompositeType::Impl, public Data
	{
	public:
		Impl(Context* context, String name, std::vector<Type> elementTypes)
			: CompositeType::Impl(context, name), Data{ std::move(elementTypes) } {}

		bool isCovariant(Type other) const override;

	protected:
		void markReference() override;
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	TupleType(Impl* impl_ptr = nullptr) : CompositeType(impl_ptr) {}

	/// 
	const std::vector<Type>& getElementTypeVector() const { return getImpl().elementTypes; }

	/// 
	Type getElementType(size_t i) const { return getImpl().elementTypes[i]; }

	/// 
	size_t getElementNum() const { return getImpl().elementTypes.size(); }
};

template<>
inline TupleType Symbol::cast() const
{
	return dynamic_cast<TupleType::Impl*>(impl_ptr);
}

/**
	* @brief 
	* @details
	* 
	* 
	*/
class StructType : public TupleType
{
public:
	/// unordered_mapkey
	struct Data
	{
		std::vector<Type> elementTypes; //!< 
		String name; //!< 
		std::vector<String> elementNames; //!< 

		DECL_HASH_EQ_NE_API(Data)
	};

protected:
	class Impl : public TupleType::Impl
	{
	public:
		Impl(Context* context, String name, std::vector<Type> elementTypes, std::vector<String> elementNames)
			: TupleType::Impl(context, name, std::move(elementTypes)), elementNames(std::move(elementNames)) {}

	protected:
		void markReference() override;

	public:
		std::vector<String> elementNames; //!< 
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	StructType(Impl* impl_ptr = nullptr) : TupleType(impl_ptr) {}

	/// 
	const std::vector<String>& getElementNameVector() const { return getImpl().elementNames; }

	/// 
	String getElementName(size_t i) const { return getImpl().elementNames[i]; }

	/// 
	VCG_API std::optional<size_t> getIndexByName(String name) const;
};

template<>
inline StructType Symbol::cast() const
{
	return dynamic_cast<StructType::Impl*>(impl_ptr);
}

STD_HASH(Type)
STD_HASH(TemplateType)
STD_HASH(CompositeType)
STD_HASH(ArrayType)
STD_HASH(TupleType)
STD_HASH(StructType)

STD_HASH(TemplateType::Data)
STD_HASH(ArrayDim::Range)
STD_HASH(ArrayDim)
STD_HASH(ArrayType::Data)
STD_HASH(TupleType::Data)
STD_HASH(StructType::Data)

class Type::Factory : public FlyweightObject::Factory
{
protected:
	explicit Factory(Context* context);
	DISABLE_COPY_MOVE(Factory)

	void eraseUseless() override;

public:
	/// 
	VCG_API Type getSimpleType(String name);

	/// 
	VCG_API TemplateType getTemplateType(String name, const std::vector<Symbol>& args);

	/// 
	VCG_API ArrayType getArrayType(Type elementType, const ArrayDim& dim);

	/// 
	VCG_API TupleType getTupleType(const std::vector<Type>& elementTypes);

	/// 
	VCG_API StructType getStructType(String name, const std::vector<Type>& elementTypes, const std::vector<String>& elementNames);

	/// 
	VCG_API Type getLogicType() const;

	/// 
	VCG_API Type getComplexType() const;

	/// 
	VCG_API Type getRealType() const;

	/// 
	VCG_API Type getRationalType() const;

	/// 
	VCG_API Type getIntegerType() const;

	/// 
	VCG_API Type getBitType() const;

	/// 
	VCG_API Type getByteType() const;

	/// 
	VCG_API TemplateType getIntType(uint64_t bits) const;

	/// 
	VCG_API TemplateType getFloatType(uint64_t exponent, uint64_t mantissa) const;

	/// 8
	TemplateType getInt8Type() const { return getIntType(8); }

	/// 16
	TemplateType getInt16Type() const { return getIntType(16); }

	/// 32
	TemplateType getInt32Type() const { return getIntType(32); }

	/// 64
	TemplateType getInt64Type() const { return getIntType(64); }

	/// IEEE754
	TemplateType getFloat16Type() const { return getFloatType(5, 11); }

	/// IEEE754
	TemplateType getFloat32Type() const { return getFloatType(8, 23); }

	/// IEEE754
	TemplateType getFloat64Type() const { return getFloatType(11, 52); }

private:
	std::unordered_map<String, Type> simplePool;
	std::unordered_map<TemplateType::Data, TemplateType> templatePool;
	std::unordered_map<ArrayType::Data, ArrayType> arrayPool;
	std::unordered_map<TupleType::Data, TupleType> tuplePool;
	std::unordered_map<StructType::Data, StructType> structPool;
};

/// 
template<typename Ret, typename... Args>
class Type::Visitor
{
public:
	Ret operator()(Type src, Args... args) const
	{
		assert(src != nullptr);
		if (auto src_CompositeType = src.cast<CompositeType>(); src_CompositeType != nullptr)
		{
			if (auto src_ArrayType = src.cast<ArrayType>(); src_ArrayType != nullptr)
			{
				if (visitArrayType != nullptr)
				{
					return visitArrayType(src_ArrayType, args...);
				}
			}
			else if (auto src_TupleType = src.cast<TupleType>(); src_TupleType != nullptr)
			{
				if (auto src_StructType = src.cast<StructType>(); src_StructType != nullptr)
				{
					if (visitStructType != nullptr)
					{
						return visitStructType(src_StructType, args...);
					}
				}
				if (visitTupleType != nullptr)
				{
					return visitTupleType(src_TupleType, args...);
				}
			}
			if (visitCompositeType != nullptr)
			{
				return visitCompositeType(src_CompositeType, args...);
			}
		}
		else if (auto src_TemplateType = src.cast<TemplateType>(); src_TemplateType != nullptr)
		{
			if (visitTemplateType != nullptr)
			{
				return visitTemplateType(src_TemplateType, args...);
			}
		}
		return defaultFunction(src, args...);
	}

public:
	std::function<Ret(Type, Args...)> defaultFunction; ///< 
	std::function<Ret(TemplateType, Args...)> visitTemplateType; ///< 
	std::function<Ret(CompositeType, Args...)> visitCompositeType; ///< 
	std::function<Ret(ArrayType, Args...)> visitArrayType; ///< 
	std::function<Ret(TupleType, Args...)> visitTupleType; ///< 
	std::function<Ret(StructType, Args...)> visitStructType; ///< 
};
