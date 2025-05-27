#pragma once

/**
 * @file Value.h
 * @brief 定义计算图顶点和边的数据结构
 */

#include "Operation.h"

class Section;
class SectionCall;

/// 所有建造者类的基类
template<typename T>
class BuilderBase
{
protected:
	DEFAULT_ALL(BuilderBase)

public:
	/// 获取实例
	T* getInstance() const { return instance; }

	/// 重置建造者
	void reset() { instance = nullptr; }

protected:
	T* instance = nullptr;
};

/**
	* @brief 作为VCG中的值类型的抽象基类
	* @details
	* 每个Value对象根据上下文可以被解释为计算图的一个或多个顶点
	*/
class Value : public MarkedObject
{
protected:
	explicit Value(Context* context) : MarkedObject(context) {}
	DEFAULT_COPY(Value)
	~Value() override = default;

	void markReference() override;

protected:
	template<typename T>
	class BuilderTemplate : public BuilderBase<T>
	{
	protected:
		DEFAULT_ALL(BuilderTemplate)

	public:
		/// 设置顶点类型
		void setType(Type type) { this->instance->type = type; }

		/// 拷贝顶点类型
		void copyType(const Value* src) { this->instance->type = src->getType(); }

		/// 设置顶点名称
		void setName(String name) { this->instance->name = name; }

		/// 拷贝顶点名称
		void copyName(const Value* src) { this->instance->name = src->getName(); }
	};

public:
	template<typename Ret, typename... Args>
	class Visitor;

	class Builder : public BuilderTemplate<Value>
	{
	public:
		DEFAULT_ALL(Builder)

		/// 由子类构造
		template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }
	};

public:
	/// 获取Value的数据类型
	Type getType() const { return type; }

	/// 获取Value的名称
	String getName() const { return name; }

	/// 获取所有依赖的Value对象
	virtual std::vector<Value*> getDependencyVector() const { return {}; }

	/// 初始化一个建造者类
	virtual Builder initBuilder() const = 0;

	/// 克隆一个建造者类
	virtual Builder cloneBuilder() const = 0;

	/// 检查是否有效
	virtual void validate() const {}

	/// 克隆一个Value对象
	Value* cloneInstance() const { return cloneBuilder().getInstance(); }

protected:
	Type type; //!< 本征属性：数据类型
	String name; //!< 名称，不是唯一标识，可以重复
};

/// 变量值（抽象类）
class VariableValue : public Value
{
protected:
	explicit VariableValue(Context* context) : Value(context) {}
	DEFAULT_COPY(VariableValue)
	~VariableValue() override = default;

	void markReference() override;

public:
	/// 获取所属的图片段
	Section* getSection() const { return section; }

protected:
	Section* section = nullptr; //!< 图结构：属于哪个图片段，nullptr表示不属于任何一个图片段

protected:
	template<typename T>
	class BuilderTemplate : public Value::BuilderTemplate<T>
	{
	protected:
		DEFAULT_ALL(BuilderTemplate)

	public:
		/// 设置所属的图片段
		void setSection(Section* section) { this->instance->section = section; }
	};
};

/**
	* @brief 输入值
	* @details
	* 作为图片段的输入形参，
	* 普通形参在实例化图片段后解释为表达式的自变量，
	* 模板形参在实例化图片段后影响图的结构。
	*/
class InputValue final : public VariableValue
{
protected:
	explicit InputValue(Context* context) : VariableValue(context) {}
	DEFAULT_COPY(InputValue)
	~InputValue() override = default;

public:
	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	/// 获取输入序号（第几个参数）
	size_t getIndex() const { return index; }

	/// 获取输入的类别
	bool isTemplate() const { return _template; }

private:
	size_t index = 0; //!< 图结构：是第几个参数
	bool _template = false; //!< 本征属性：属于那种类别的输入，是否模板形参

public:
	class Builder : public VariableValue::BuilderTemplate<InputValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API InputValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API InputValue* cloneInstance(const InputValue* src);

		/// 初始化一个实例
		VCG_API InputValue* initInstance(const Value* src);

		/// 从基类建造者设置实例
		InputValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<InputValue*>(src.getInstance()); }

		/// 设置输入序号（第几个参数）
		void setIndex(size_t index) { this->instance->index = index; }

		/// 设置输入的类别
		void setTemplate(bool val) { this->instance->_template = val; }
	};
};

/**
	* @brief 约束变元值
	* @details
	* 作为图片段生成器的约束变元，
	* 约束变元在实例化图片段后创建多个顶点实例。
	*/
class BoundValue final : public VariableValue
{
protected:
	explicit BoundValue(Context* context) : VariableValue(context) {}
	DEFAULT_COPY(BoundValue)
	~BoundValue() override = default;

public:
	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	/// 获取输入序号（第几个参数）
	size_t getIndex() const { return index; }

private:
	size_t index = 0; //!< 图结构：是第几个参数

public:
	class Builder : public VariableValue::BuilderTemplate<BoundValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API BoundValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API BoundValue* cloneInstance(const BoundValue* src);

		/// 从基类建造者设置实例
		BoundValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<BoundValue*>(src.getInstance()); }

		/// 设置输入序号（第几个参数）
		void setIndex(size_t index) { this->instance->index = index; }
	};
};

/**
	* @brief 操作值
	* @details
	* 源操作数经过某种操作后得到的结果，源操作数无效或不满足特定条件会产生异常
	* 可选条件执行，不满足条件得到无效值，不产生异常
	*/
class OperationValue final : public VariableValue
{
protected:
	explicit OperationValue(Context* context) : VariableValue(context) {}
	DEFAULT_COPY(OperationValue)
	~OperationValue() override = default;

	void markReference() override;

public:
	std::vector<Value*> getDependencyVector() const override { return srcVector; }

	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	VCG_API void validate() const override;

	/// 获取操作码
	Operation getOperation() const { return op; }

	/// 获取所有源操作数
	const std::vector<Value*>& getSrcVector() const { return srcVector; }

	/// 根据索引获取某个源操作数
	Value* getSrc(size_t i) const { return srcVector[i]; }

	/// 获取源操作数数量
	size_t getSrcVectorSize() const { return srcVector.size(); }

private:
	Operation op; //!< 本征属性：操作码
	std::vector<Value*> srcVector; //!< 图结构：源操作数是哪些顶点，顺序是边的本征属性

public:
	class Builder : public VariableValue::BuilderTemplate<OperationValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API OperationValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API OperationValue* cloneInstance(const OperationValue* src);

		/// 从基类建造者设置实例
		OperationValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<OperationValue*>(src.getInstance()); }

		/// 设置操作码
		void setOperation(Operation op) { this->instance->op = op; }

		/// 设置操作码并以返回值类型来设置类型
		void setOperationAndType(Operation op) { setOperation(op); setType(op.getReturnType()); }

		/// 设置所有源操作数
		void setSrcVector(std::vector<Value*> srcVector) { this->instance->srcVector = std::move(srcVector); }

		/// 根据索引设置某个源操作数
		void setSrc(size_t i, Value* src) { this->instance->srcVector[i] = src; }

		/// 设置源操作数数量
		void setSrcVectorSize(size_t n) { this->instance->srcVector.resize(n, nullptr); }
	};
};

/**
	* @brief 结果值
	* @details
	* 用于获取图片段的输出，在实例化图片段时解释为复制操作
	*/
class ResultValue final : public VariableValue
{
protected:
	explicit ResultValue(Context* context) : VariableValue(context) {}
	DEFAULT_COPY(ResultValue)
	~ResultValue() override = default;

	void markReference() override;

public:
	std::vector<Value*> getDependencyVector() const override { return { src }; }

	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	VCG_API void validate() const override;

	/// 获取调用者
	SectionCall* getCall() const { return call; }

	/// 获取数据源
	VariableValue* getSrc() const { return src; }

private:
	SectionCall* call = nullptr; //!< 图结构：所属的调用实例
	VariableValue* src = nullptr; //!< 图结构：数据源是哪个顶点

public:
	class Builder : public VariableValue::BuilderTemplate<ResultValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API ResultValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API ResultValue* cloneInstance(const ResultValue* src);

		/// 从基类建造者设置实例
		ResultValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<ResultValue*>(src.getInstance()); }

		/// 设置调用者
		void setCall(SectionCall* call) { this->instance->call = call; }

		/// 设置数据源
		void setSrc(VariableValue* src) { this->instance->src = src; }

		/// 设置数据源并以数据源类型来设置类型
		void setSrcAndType(VariableValue* src) { setSrc(src); copyType(src); }
	};
};

/**
	* @brief 分支合流值
	* @details
	* 用于分支和循环生成器的表示数据流合流，在实例化图片段后解释为条件选择值
	*/
class PhiValue final : public VariableValue
{
protected:
	explicit PhiValue(Context* context) : VariableValue(context) {}
	DEFAULT_COPY(PhiValue)
	~PhiValue() override = default;

	void markReference() override;

public:
	std::vector<Value*> getDependencyVector() const override { return incomingVector; }

	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	VCG_API void validate() const override;

	/// 获取所有入边
	const std::vector<Value*>& getIncomingVector() const { return incomingVector; }

	/// 根据索引获取某个入边
	Value* getIncoming(size_t i) const { return incomingVector[i]; }

	/// 获取入边数量
	size_t getIncomingVectorSize() const { return incomingVector.size(); }

private:
	std::vector<Value*> incomingVector; //!< 图结构：所有依赖的边

public:
	class Builder : public VariableValue::BuilderTemplate<PhiValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API PhiValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API PhiValue* cloneInstance(const PhiValue* src);

		/// 从基类建造者设置实例
		PhiValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<PhiValue*>(src.getInstance()); }

		/// 设置所有入边
		void setIncomingVector(std::vector<Value*> incomingVector) { this->instance->incomingVector = std::move(incomingVector); }

		/// 根据索引设置某个入边
		void setIncoming(size_t i, Value* incoming) { this->instance->incomingVector[i] = incoming; }

		/// 设置入边数量（缺省值是2）
		void setIncomingVectorSize(size_t n) { this->instance->incomingVector.resize(n, nullptr); }
	};
};

/**
	* @brief 常量值
	* @details
	* 用一个符号来表示的常量，具有固定的数值
	*/
class ConstantValue final : public Value
{
protected:
	explicit ConstantValue(Context* context) : Value(context) {}
	DEFAULT_COPY(ConstantValue)
	~ConstantValue() override = default;

	void markReference() override;

public:
	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

	/// 获取值
	Symbol getValue() const { return value; }

	/// 获取子类值
	template<typename T>
	T getValue() const { return value.cast<T>(); }

private:
	Symbol value; //!< 本征属性：保存具体的数值

public:
	class Builder : public Value::BuilderTemplate<ConstantValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API ConstantValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API ConstantValue* cloneInstance(const ConstantValue* src);

		/// 从基类建造者设置实例
		ConstantValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<ConstantValue*>(src.getInstance()); }

		/// 设置值
		void setValue(Symbol value) { this->instance->value = value; }
	};
};

/**
	* @brief 无效值
	* @details
	* 表示一个取值范围为空的顶点，是一种占位符，
	* 可以用于支持条件执行，也可以给不需要使用的参数赋此值
	*/
class InvalidValue final : public Value
{
protected:
	explicit InvalidValue(Context* context) : Value(context) {}
	DEFAULT_COPY(InvalidValue)
	~InvalidValue() override = default;

public:
	VCG_API Value::Builder initBuilder() const override;

	VCG_API Value::Builder cloneBuilder() const override;

public:
	class Builder : public Value::BuilderTemplate<InvalidValue>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API InvalidValue* createInstance(Context* context);

		/// 克隆一个实例
		VCG_API InvalidValue* cloneInstance(const InvalidValue* src);

		/// 从基类建造者设置实例
		InvalidValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<InvalidValue*>(src.getInstance()); }
	};
};

/// 根据类型子类确定调用哪个函数的访问者类模板
template<typename Ret, typename... Args>
class Value::Visitor
{
public:
	Ret operator()(Value* src, Args... args) const
	{
		assert(src != nullptr);
		if (auto src_VariableValue = dynamic_cast<VariableValue*>(src))
		{
			if (auto src_InputValue = dynamic_cast<InputValue*>(src))
			{
				if (visitInputValue != nullptr)
				{
					return visitInputValue(src_InputValue, args...);
				}
			}
			else if (auto src_BoundValue = dynamic_cast<BoundValue*>(src))
			{
				if (visitBoundValue != nullptr)
				{
					return visitBoundValue(src_BoundValue, args...);
				}
			}
			else if (auto src_OperationValue = dynamic_cast<OperationValue*>(src))
			{
				if (visitOperationValue != nullptr)
				{
					return visitOperationValue(src_OperationValue, args...);
				}
			}
			else if (auto src_ResultValue = dynamic_cast<ResultValue*>(src))
			{
				if (visitResultValue != nullptr)
				{
					return visitResultValue(src_ResultValue, args...);
				}
			}
			else if (auto src_PhiValue = dynamic_cast<PhiValue*>(src))
			{
				if (visitPhiValue != nullptr)
				{
					return visitPhiValue(src_PhiValue, args...);
				}
			}
			if (visitVariableValue != nullptr)
			{
				return visitVariableValue(src_VariableValue, args...);
			}
		}
		else if (auto src_ConstantValue = dynamic_cast<ConstantValue*>(src))
		{
			if (visitConstantValue != nullptr)
			{
				return visitConstantValue(src_ConstantValue, args...);
			}
		}
		else if (auto src_InvalidValue = dynamic_cast<InvalidValue*>(src))
		{
			if (visitInvalidValue != nullptr)
			{
				return visitInvalidValue(src_InvalidValue, args...);
			}
		}
		return defaultFunction(src, args...);
	}

public:
	std::function<Ret(Value*, Args...)> defaultFunction; ///< 未查到相应处理函数时的默认处理函数
	std::function<Ret(VariableValue*, Args...)> visitVariableValue; ///< 变量值的处理函数
	std::function<Ret(InputValue*, Args...)> visitInputValue; ///< 输入值的处理函数
	std::function<Ret(BoundValue*, Args...)> visitBoundValue; ///< 约束变元的处理函数
	std::function<Ret(OperationValue*, Args...)> visitOperationValue; ///< 操作值的处理函数
	std::function<Ret(ResultValue*, Args...)> visitResultValue; ///< 结果值的处理函数
	std::function<Ret(PhiValue*, Args...)> visitPhiValue; ///< 分支合流值的处理函数
	std::function<Ret(ConstantValue*, Args...)> visitConstantValue; ///< 常量值的处理函数
	std::function<Ret(InvalidValue*, Args...)> visitInvalidValue; ///< 无效值的处理函数
};
