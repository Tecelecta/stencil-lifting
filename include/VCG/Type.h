#pragma once

/**
 * @file Type.h
 * @brief 定义类型系统的数据结构
 */

#include "AnyString.h"
#include "Enum.h"
#include "BitVector.h"
#include "Integer.h"
#include "Decimal.h"

#include <optional>

/**
	* @brief 作为VCG中的数据类型的统一接口
	* @details
	* 每个数据类型由名称和模板参数组成，在内存中使用 Type::Impl 类表示，然后使用该类的作为接口。
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
		String name; //!< 类型名称，区分不同类型。名称应当是对该类型的结构和用途的解释。模板类型需要模板参数也相同才是同一个类型。
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;

public:
	Type(Impl* impl_ptr = nullptr) : Symbol(impl_ptr) {}

	/// 获取类型名称
	String getName() const { return getImpl().name; }

	/// 判断在VCG中是否直接或间接继承自另一个类型
	VCG_API bool inheritsFrom(Type other) const;

	/// 判断在VCG中是否可以与另一个类型协变
	bool isCovariant(Type other) const { return getImpl().isCovariant(other); }

	/**
		* @brief 判断在VCG中是否可以作为实参，传给形参为另一个类型的函数
		* @details
		* 包括相等、继承、协变。
		*/
	bool canBeArgument(Type other) const { return *this == other || inheritsFrom(other) || isCovariant(other); }

	/**
		* @brief 获取数组维数
		* @return 对于数组类型，返回维数
		* @return 对于非数组类型，返回0
		*/
	VCG_API size_t getNumDims() const;

	/**
		* @brief 获取元素数量
		* @return 对于元组和定长数组类型，返回元素数量
		* @return 对于变长数组类型，返回null
		* @return 对于非复合类型，返回1
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
	* @brief 模板类型
	* @details
	* 模板类型是在类型名的基础上附加更多参数得到的，增加定义类型时的表达能力。
	*/
class TemplateType : public Type
{
public:
	/// 区别不同的模板类型，可以作为unordered_map的key
	struct Data
	{
		String name; //!< 模板类型名称
		std::vector<Symbol> args; //!< 模板参数

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
		std::vector<Symbol> argumentVector; //!< 模板参数
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	TemplateType(Impl* impl_ptr = nullptr) : Type(impl_ptr) {}

	/// 获取所有实参
	const std::vector<Symbol>& getArgumentVector() const { return getImpl().argumentVector; }

	/// 根据索引获取某个实参
	Symbol getArgument(size_t i) const { return getImpl().argumentVector[i]; }

	/// 获取实参数量
	size_t getArgumentVectorSize() const { return getImpl().argumentVector.size(); }

	friend Type::Factory;
};

template<>
inline TemplateType Symbol::cast<TemplateType>() const
{
	return dynamic_cast<TemplateType::Impl*>(impl_ptr);
}

/**
	* @brief 复合类型
	* @details
	* 复合类型是由其他的类型复合而成，支持解构操作。
	* 一般的复合类型仅是元素之间的简单聚合，有数组和元组两种基本复合方式。
	* 对复合类型的每个成员给出特定的解释，或者规定成员之间的关系，可以派生出特化的复合类型。
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

/// 表示数组的维度
struct ArrayDim
{
	/// 表示数组的一个维度的范围
	struct Range
	{
		Integer lower;
		Integer upper;

		DECL_HASH_EQ_NE_API(Range)
	};

	/**
		* @brief 创建纯动态数组类型
		* @param numDims 数组维度
		*/
	VCG_API ArrayDim(size_t numDims);

	/**
		* @brief 创建C风格（从0开始）的静态数组或动静混搭数组
		* @param data 每一维的长度，null值表示动态
		*/
	VCG_API ArrayDim(const std::vector<Integer>& data); // 的纯静态数组

	/**
		* @brief 创建Fortran风格的静态数组或动静混搭数组
		* @param data 每一维的下界(first)和上界(second)，null值表示动态
		*/
	VCG_API ArrayDim(std::vector<Range> data);

	static constexpr size_t MAX_NUM_DIMS = 100;

	/// 获取维度数量
	size_t getNumDims() const { return data.size(); }

	/// 访问某一维
	Range& operator[](size_t i) { return data[i]; }

	/// 常量访问某一维
	const Range& operator[](size_t i) const { return data[i]; }

	/// 以SDSL格式打印
	std::string toString() const;

	/// 数组每一维的范围
	std::vector<Range> data;

	DECL_HASH_EQ_NE_API(ArrayDim)
};

/**
	* @brief 数组类型
	* @details
	* 数组类型是由若干个相同的类型复合而成，是基本复合方式之一。
	* 支持定长、变长和混合长度。支持对元素的随机访问。
	*/
class ArrayType : public CompositeType
{
public:
	/// 区别不同的数组类型，可以作为unordered_map的key
	struct Data
	{
		Type elementType; //!< 数组元素的类型
		ArrayDim dim; //!< 数组维度

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

	/// 获取数组元素类型
	Type getElementType() const { return getImpl().elementType; }

	/// 获取数组维度
	const ArrayDim& getDimObject() const { return getImpl().dim; }

	/// 获取数组某一维
	const ArrayDim::Range& getDim(size_t i) const { return getDimObject()[i]; }

	/// 获取数组有几维
	size_t getNumDims() const { return getDimObject().getNumDims(); }
};

template<>
inline ArrayType Symbol::cast() const
{
	return dynamic_cast<ArrayType::Impl*>(impl_ptr);
}

/**
	* @brief 元组类型
	* @details
	* 元组类型是由有限个可以不同的类型复合而成，是基本复合方式之一。
	* 对于每个特定的元组类型，元素的数量和类型是不可再更改的。
	*/
class TupleType : public CompositeType
{
public:
	/// 区别不同的元组类型，可以作为unordered_map的key
	struct Data
	{
		std::vector<Type> elementTypes; //!< 元组元素的类型

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

	/// 获取所有元素类型
	const std::vector<Type>& getElementTypeVector() const { return getImpl().elementTypes; }

	/// 根据索引获取某个元素类型
	Type getElementType(size_t i) const { return getImpl().elementTypes[i]; }

	/// 获取元素数量
	size_t getElementNum() const { return getImpl().elementTypes.size(); }
};

template<>
inline TupleType Symbol::cast() const
{
	return dynamic_cast<TupleType::Impl*>(impl_ptr);
}

/**
	* @brief 结构体类型
	* @details
	* 结构体类型是由元组类型，为每个元素给定解释得到。
	* 相比于元组类型，结构体的元素关系更加紧密，具有的内聚性。
	*/
class StructType : public TupleType
{
public:
	/// 区别不同的结构体类型，可以作为unordered_map的key
	struct Data
	{
		std::vector<Type> elementTypes; //!< 结构体元素的类型
		String name; //!< 结构体类型名称
		std::vector<String> elementNames; //!< 结构体成员名称

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
		std::vector<String> elementNames; //!< 结构体成员名称
	};

	Impl& getImpl() const { return *reinterpret_cast<Impl*>(impl_ptr); }

	template<typename T> friend T Symbol::cast() const;
	friend Type::Factory;

public:
	StructType(Impl* impl_ptr = nullptr) : TupleType(impl_ptr) {}

	/// 获取所有成员名称
	const std::vector<String>& getElementNameVector() const { return getImpl().elementNames; }

	/// 根据索引获取某个成员名称
	String getElementName(size_t i) const { return getImpl().elementNames[i]; }

	/// 根据某个成员名称获取索引
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
	/// 获取简单类型对象
	VCG_API Type getSimpleType(String name);

	/// 获取模板类型对象
	VCG_API TemplateType getTemplateType(String name, const std::vector<Symbol>& args);

	/// 获取数组类型对象
	VCG_API ArrayType getArrayType(Type elementType, const ArrayDim& dim);

	/// 获取元组类型对象
	VCG_API TupleType getTupleType(const std::vector<Type>& elementTypes);

	/// 获取结构体类型对象
	VCG_API StructType getStructType(String name, const std::vector<Type>& elementTypes, const std::vector<String>& elementNames);

	/// 获取逻辑类型对象
	VCG_API Type getLogicType() const;

	/// 获取复数类型对象
	VCG_API Type getComplexType() const;

	/// 获取实数类型对象
	VCG_API Type getRealType() const;

	/// 获取有理数类型对象
	VCG_API Type getRationalType() const;

	/// 获取整数类型对象
	VCG_API Type getIntegerType() const;

	/// 获取比特类型对象
	VCG_API Type getBitType() const;

	/// 获取字节类型对象
	VCG_API Type getByteType() const;

	/// 获取指定位数整型对象
	VCG_API TemplateType getIntType(uint64_t bits) const;

	/// 获取指定精度的浮点型对象
	VCG_API TemplateType getFloatType(uint64_t exponent, uint64_t mantissa) const;

	/// 获取8位整型对象
	TemplateType getInt8Type() const { return getIntType(8); }

	/// 获取16位整型对象
	TemplateType getInt16Type() const { return getIntType(16); }

	/// 获取32位整型对象
	TemplateType getInt32Type() const { return getIntType(32); }

	/// 获取64位整型对象
	TemplateType getInt64Type() const { return getIntType(64); }

	/// 获取IEEE754标准半精度浮点型对象
	TemplateType getFloat16Type() const { return getFloatType(5, 11); }

	/// 获取IEEE754标准单精度浮点型对象
	TemplateType getFloat32Type() const { return getFloatType(8, 23); }

	/// 获取IEEE754标准双精度浮点型对象
	TemplateType getFloat64Type() const { return getFloatType(11, 52); }

private:
	std::unordered_map<String, Type> simplePool;
	std::unordered_map<TemplateType::Data, TemplateType> templatePool;
	std::unordered_map<ArrayType::Data, ArrayType> arrayPool;
	std::unordered_map<TupleType::Data, TupleType> tuplePool;
	std::unordered_map<StructType::Data, StructType> structPool;
};

/// 根据类型子类确定调用哪个函数的访问者类模板
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
	std::function<Ret(Type, Args...)> defaultFunction; ///< 未查到相应处理函数时的默认处理函数
	std::function<Ret(TemplateType, Args...)> visitTemplateType; ///< 模板类型的处理函数
	std::function<Ret(CompositeType, Args...)> visitCompositeType; ///< 复合类型的处理函数
	std::function<Ret(ArrayType, Args...)> visitArrayType; ///< 数组类型的处理函数
	std::function<Ret(TupleType, Args...)> visitTupleType; ///< 元组类型的处理函数
	std::function<Ret(StructType, Args...)> visitStructType; ///< 结构体类型的处理函数
};
