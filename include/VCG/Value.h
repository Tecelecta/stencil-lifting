#pragma once

/**
 * @file Value.h
 * @brief 
 */

#include "Operation.h"

class Section;
class SectionCall;

/// 
template<typename T>
class BuilderBase
{
protected:
	DEFAULT_ALL(BuilderBase)

public:
	/// 
	T* getInstance() const { return instance; }

	/// 
	void reset() { instance = nullptr; }

protected:
	T* instance = nullptr;
};

/**
	* @brief VCG
	* @details
	* Value
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
		/// 
		void setType(Type type) { this->instance->type = type; }

		/// 
		void copyType(const Value* src) { this->instance->type = src->getType(); }

		/// 
		void setName(String name) { this->instance->name = name; }

		/// 
		void copyName(const Value* src) { this->instance->name = src->getName(); }
	};

public:
	template<typename Ret, typename... Args>
	class Visitor;

	class Builder : public BuilderTemplate<Value>
	{
	public:
		DEFAULT_ALL(Builder)

		/// 
		template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }
	};

public:
	/// Value
	Type getType() const { return type; }

	/// Value
	String getName() const { return name; }

	/// Value
	virtual std::vector<Value*> getDependencyVector() const { return {}; }

	/// 
	virtual Builder initBuilder() const = 0;

	/// 
	virtual Builder cloneBuilder() const = 0;

	/// 
	virtual void validate() const {}

	/// Value
	Value* cloneInstance() const { return cloneBuilder().getInstance(); }

protected:
	Type type; //!< 
	String name; //!< 
};

/// 
class VariableValue : public Value
{
protected:
	explicit VariableValue(Context* context) : Value(context) {}
	DEFAULT_COPY(VariableValue)
	~VariableValue() override = default;

	void markReference() override;

public:
	/// 
	Section* getSection() const { return section; }

protected:
	Section* section = nullptr; //!< nullptr

protected:
	template<typename T>
	class BuilderTemplate : public Value::BuilderTemplate<T>
	{
	protected:
		DEFAULT_ALL(BuilderTemplate)

	public:
		/// 
		void setSection(Section* section) { this->instance->section = section; }
	};
};

/**
	* @brief 
	* @details
	* 
	* 
	* 
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

	/// 
	size_t getIndex() const { return index; }

	/// 
	bool isTemplate() const { return _template; }

private:
	size_t index = 0; //!< 
	bool _template = false; //!< 

public:
	class Builder : public VariableValue::BuilderTemplate<InputValue>
	{
	public:
		/// 
		VCG_API InputValue* createInstance(Context* context);

		/// 
		VCG_API InputValue* cloneInstance(const InputValue* src);

		/// 
		VCG_API InputValue* initInstance(const Value* src);

		/// 
		InputValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<InputValue*>(src.getInstance()); }

		/// 
		void setIndex(size_t index) { this->instance->index = index; }

		/// 
		void setTemplate(bool val) { this->instance->_template = val; }
	};
};

/**
	* @brief 
	* @details
	* 
	* 
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

	/// 
	size_t getIndex() const { return index; }

private:
	size_t index = 0; //!< 

public:
	class Builder : public VariableValue::BuilderTemplate<BoundValue>
	{
	public:
		/// 
		VCG_API BoundValue* createInstance(Context* context);

		/// 
		VCG_API BoundValue* cloneInstance(const BoundValue* src);

		/// 
		BoundValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<BoundValue*>(src.getInstance()); }

		/// 
		void setIndex(size_t index) { this->instance->index = index; }
	};
};

/**
	* @brief 
	* @details
	* 
	* 
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

	/// 
	Operation getOperation() const { return op; }

	/// 
	const std::vector<Value*>& getSrcVector() const { return srcVector; }

	/// 
	Value* getSrc(size_t i) const { return srcVector[i]; }

	/// 
	size_t getSrcVectorSize() const { return srcVector.size(); }

private:
	Operation op; //!< 
	std::vector<Value*> srcVector; //!< 

public:
	class Builder : public VariableValue::BuilderTemplate<OperationValue>
	{
	public:
		/// 
		VCG_API OperationValue* createInstance(Context* context);

		/// 
		VCG_API OperationValue* cloneInstance(const OperationValue* src);

		/// 
		OperationValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<OperationValue*>(src.getInstance()); }

		/// 
		void setOperation(Operation op) { this->instance->op = op; }

		/// 
		void setOperationAndType(Operation op) { setOperation(op); setType(op.getReturnType()); }

		/// 
		void setSrcVector(std::vector<Value*> srcVector) { this->instance->srcVector = std::move(srcVector); }

		/// 
		void setSrc(size_t i, Value* src) { this->instance->srcVector[i] = src; }

		/// 
		void setSrcVectorSize(size_t n) { this->instance->srcVector.resize(n, nullptr); }
	};
};

/**
	* @brief 
	* @details
	* 
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

	/// 
	SectionCall* getCall() const { return call; }

	/// 
	VariableValue* getSrc() const { return src; }

private:
	SectionCall* call = nullptr; //!< 
	VariableValue* src = nullptr; //!< 

public:
	class Builder : public VariableValue::BuilderTemplate<ResultValue>
	{
	public:
		/// 
		VCG_API ResultValue* createInstance(Context* context);

		/// 
		VCG_API ResultValue* cloneInstance(const ResultValue* src);

		/// 
		ResultValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<ResultValue*>(src.getInstance()); }

		/// 
		void setCall(SectionCall* call) { this->instance->call = call; }

		/// 
		void setSrc(VariableValue* src) { this->instance->src = src; }

		/// 
		void setSrcAndType(VariableValue* src) { setSrc(src); copyType(src); }
	};
};

/**
	* @brief 
	* @details
	* 
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

	/// 
	const std::vector<Value*>& getIncomingVector() const { return incomingVector; }

	/// 
	Value* getIncoming(size_t i) const { return incomingVector[i]; }

	/// 
	size_t getIncomingVectorSize() const { return incomingVector.size(); }

private:
	std::vector<Value*> incomingVector; //!< 

public:
	class Builder : public VariableValue::BuilderTemplate<PhiValue>
	{
	public:
		/// 
		VCG_API PhiValue* createInstance(Context* context);

		/// 
		VCG_API PhiValue* cloneInstance(const PhiValue* src);

		/// 
		PhiValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<PhiValue*>(src.getInstance()); }

		/// 
		void setIncomingVector(std::vector<Value*> incomingVector) { this->instance->incomingVector = std::move(incomingVector); }

		/// 
		void setIncoming(size_t i, Value* incoming) { this->instance->incomingVector[i] = incoming; }

		/// 2
		void setIncomingVectorSize(size_t n) { this->instance->incomingVector.resize(n, nullptr); }
	};
};

/**
	* @brief 
	* @details
	* 
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

	/// 
	Symbol getValue() const { return value; }

	/// 
	template<typename T>
	T getValue() const { return value.cast<T>(); }

private:
	Symbol value; //!< 

public:
	class Builder : public Value::BuilderTemplate<ConstantValue>
	{
	public:
		/// 
		VCG_API ConstantValue* createInstance(Context* context);

		/// 
		VCG_API ConstantValue* cloneInstance(const ConstantValue* src);

		/// 
		ConstantValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<ConstantValue*>(src.getInstance()); }

		/// 
		void setValue(Symbol value) { this->instance->value = value; }
	};
};

/**
	* @brief 
	* @details
	* 
	* 
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
		/// 
		VCG_API InvalidValue* createInstance(Context* context);

		/// 
		VCG_API InvalidValue* cloneInstance(const InvalidValue* src);

		/// 
		InvalidValue* setInstance(const Value::Builder& src) { return instance = dynamic_cast<InvalidValue*>(src.getInstance()); }
	};
};

/// 
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
	std::function<Ret(Value*, Args...)> defaultFunction; ///< 
	std::function<Ret(VariableValue*, Args...)> visitVariableValue; ///< 
	std::function<Ret(InputValue*, Args...)> visitInputValue; ///< 
	std::function<Ret(BoundValue*, Args...)> visitBoundValue; ///< 
	std::function<Ret(OperationValue*, Args...)> visitOperationValue; ///< 
	std::function<Ret(ResultValue*, Args...)> visitResultValue; ///< 
	std::function<Ret(PhiValue*, Args...)> visitPhiValue; ///< 
	std::function<Ret(ConstantValue*, Args...)> visitConstantValue; ///< 
	std::function<Ret(InvalidValue*, Args...)> visitInvalidValue; ///< 
};
