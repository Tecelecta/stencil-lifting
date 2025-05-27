#pragma once

/**
 * @file Section.h
 * @brief 定义计算图的构建和片段复用方式
 */

#include "Value.h"

/// 实例化一个子结构，相当于函数调用
class SectionCall : public MarkedObject
{
protected:
	explicit SectionCall(Context* context) : MarkedObject(context) {}
	DEFAULT_COPY(SectionCall)
	~SectionCall() override = default;

	void markReference() override;

public:
	/// 获取调用方
	Section* getCaller() const { return caller; }

	/// 获取被调用方
	Section* getCallee() const { return callee; }

	/// 获取调用序号
	size_t getIndex() const { return index; }

	/// 获取所有实参
	const std::vector<Value*>& getArgumentVector() const { return argumentVector; }

	/// 根据索引获取某个实参
	Value* getArgument(size_t i) const { return argumentVector[i]; }

	/// 获取实参数量
	size_t getArgumentVectorSize() const { return argumentVector.size(); }

	/// 获取所有特化实参
	const std::vector<Value*>& getSpecializerVector() const { return specializerVector; }

	/// 根据索引获取某个特化实参
	Value* getSpecializer(size_t i) const { return specializerVector[i]; }

	/// 获取特化实参数量
	size_t getSpecializerVectorSize() const { return specializerVector.size(); }

	/// 获取所有结果值
	const std::vector<ResultValue*>& getResultValueVector() const { return resultValueVector; }

	/// 根据索引获取某个结果值
	ResultValue* getResultValue(size_t i) const { return resultValueVector[i]; }

	/// 获取结果值数量
	size_t getResultValueVectorSize() const { return resultValueVector.size(); }

	/// 检查是否有效
	VCG_API void validate() const;

private:
	Section* caller = nullptr; //!< 调用方
	Section* callee = nullptr; //!< 被调用方
	size_t index = 0; //!< 是第几个调用
	std::vector<Value*> argumentVector; /// 实参，对应parameter形参
	std::vector<Value*> specializerVector; /// 特化实参，对应template形参
	std::vector<ResultValue*> resultValueVector; //!< 保存结果值，元素顺序不影响图结构，与序列化顺序保持一致

public:
	class Builder : public BuilderBase<SectionCall>
	{
	public:
		/**
			* @brief 根据被调用方，创建一个图片段调用实例
			* @details
			* 此方法自动设置argumentVector和specializerVector的大小
			*/
		VCG_API SectionCall* createInstance(Section* callee);

		/// 克隆一个实例
		VCG_API SectionCall* cloneInstance(SectionCall* src);

		/**
			* @brief 设置调用方
			* @details
			* 先调用此方法，再设置结果值。
			* 向Section中添加调用的时候会自动调用该方法。
			*/
		void setCaller(Section* caller) { this->instance->caller = caller; }

		/// 设置调用序号（第几个调用）
		void setIndex(size_t index) { this->instance->index = index; }

		/// 根据索引设置某个实参
		void setArgument(size_t i, Value* value) { this->instance->argumentVector[i] = value; }

		/// 根据索引设置某个特化实参
		void setSpecializer(size_t i, Value* value) { this->instance->specializerVector[i] = value; }

		/// 添加一个结果值
		VCG_API void addResultValue(ResultValue::Builder& builder);

		/// 根据数据源创建一个结果值
		VCG_API ResultValue* createResultValue(VariableValue* src, String name = String());
	};
};

/**
	* @brief 组成计算图的片段
	* @details
	* 每个图片段被实例化后，在调用方视角看得到一个子图
	* @see SectionCall
	*/
class Section : public MarkedObject
{
protected:
	explicit Section(Context* context) : MarkedObject(context) {}
	DISABLE_COPY(Section)
	~Section() override = default;

	void markReference() override;

public:
	template<typename T>
	class BuilderTemplate : public BuilderBase<T>
	{
	public:
		/// 添加一个形参
		void addParameter(InputValue::Builder& builder)
		{
			builder.setIndex((uint32_t)this->instance->parameterVector.size());
			this->instance->parameterVector.push_back(builder.getInstance());
			builder.setSection(this->instance);
			builder.setTemplate(false);
		}

		/// 设置形参数量
		void setParameterVectorSize(size_t n)
		{
			this->instance->parameterVector.resize(n, nullptr);
		}

		/// 根据索引设置某个形参
		void setParameter(uint32_t i, InputValue::Builder& builder)
		{
			this->instance->parameterVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
			builder.setTemplate(false);
		}

		/// 创建一个形参
		InputValue* createParameter(Type type, String name = String())
		{
			InputValue::Builder builder;
			builder.createInstance(this->instance->getContext());
			builder.setType(type);
			builder.setName(name);
			addParameter(builder);
			return builder.getInstance();
		}

		/// 创建一个形参
		InputValue* createParameter(Type type, std::string_view name)
		{
			return createParameter(type, this->instance->getContext()->getString(name));
		}

		/// 根据索引设置某个图片段调用
		void setSectionCall(size_t i, SectionCall::Builder& builder)
		{
			this->instance->sectionCallVector[i] = builder.getInstance();
			builder.setIndex(i);
			builder.setCaller(this->instance);
		}

		/// 设置图片段调用数量
		void setSectionCallVectorSize(size_t n) { this->instance->sectionCallVector.resize(n, nullptr); }

		/// 添加一个图片段调用
		void addSectionCall(SectionCall::Builder& builder)
		{
			builder.setIndex(this->instance->sectionCallVector.size());
			this->instance->sectionCallVector.push_back(builder.getInstance());
			builder.setCaller(this->instance);
		}

		/// 设置图片段名称
		void setSectionName(String name) { this->instance->sectionName = name == nullptr ? this->instance->getContext()->getEmptyString() : name; }

		/// 设置图片段名称
		void setSectionName(std::string_view name) { this->instance->sectionName = this->instance->getContext()->getString(name); }

		/// 复制图片段名称
		void copySectionName(Section* src) { this->instance->sectionName = src->getName(); }
	};

	class Builder : public BuilderTemplate<Section>
	{
	public:
		DEFAULT_ALL(Builder)

		/// 由子类构造
		template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }
	};

public:
	/// 获取所有形参
	const std::vector<InputValue*>& getParameterVector() const { return parameterVector; }

	/// 根据索引获取某个形参
	InputValue* getParameter(size_t i) const { return parameterVector[i]; }

	/// 获取形参数量
	size_t getParameterVectorSize() const { return parameterVector.size(); }

	/// 获取所有图片段调用
	const std::vector<SectionCall*>& getSectionCallVector() const { return sectionCallVector; }

	/// 根据索引获取某个图片段调用
	SectionCall* getSectionCall(size_t i) const { return sectionCallVector[i]; }

	/// 获取图片段调用数量
	size_t getSectionCallVectorSize() const { return sectionCallVector.size(); }

	/// 获取获取图片段名称
	String getName() const { return sectionName; }

	/// 获取所有包含的变量
	virtual VCG_API std::vector<VariableValue*> getVariableVector() const;

	/// 获取子类名称
	virtual const char* getCategory() const = 0;

	/// 初始化一个建造者类
	virtual Builder initBuilder() const = 0;

	/// 检查是否有效
	VCG_API void validate() const;

protected:
	std::vector<InputValue*> parameterVector; //!< 图结构：普通形参，与序列化顺序保持一致
	std::vector<SectionCall*> sectionCallVector; //!< 保存图片段调用，元素顺序不影响图结构，与序列化顺序保持一致
	String sectionName; //!< Section的名字（类似函数名）
};

/// 简单的图片段
class SimpleSection final : public Section
{
protected:
	SimpleSection(Context* context) : Section(context) {}
	~SimpleSection() override = default;

	void markReference() override;

public:
	class Builder;

	VCG_API std::vector<VariableValue*> getVariableVector() const override;

	VCG_API Section::Builder initBuilder() const override;

	/// 获取所有操作值
	const std::vector<OperationValue*>& getOperationValueVector() const { return operationValueVector; }

	/// 根据索引获取某个操作值
	OperationValue* getOperationValue(size_t i) const { return operationValueVector[i]; }

	/// 获取操作值数量
	size_t getOperationValueVectorSize() const { return operationValueVector.size(); }

	/// 获取子类名称
	const char* getCategory() const override { return "SimpleSection"; }

private:
	std::vector<OperationValue*> operationValueVector; //!< 保存操作值，元素顺序不影响图结构，与序列化顺序保持一致

public:
	class Builder : public Section::BuilderTemplate<SimpleSection>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API SimpleSection* createInstance(Context* context);

		/// 从基类建造者设置实例
		SimpleSection* setInstance(const Section::Builder& src) { return instance = dynamic_cast<SimpleSection*>(src.getInstance()); }

		/// 添加一个操作值
		void addOperationValue(OperationValue::Builder& builder)
		{
			instance->operationValueVector.push_back(builder.getInstance());
			builder.setSection(this->instance);
			//builder.getInstance()->validate();
		}

		/// 创建一个操作值
		VCG_API OperationValue* createOperationValue(Operation op, std::vector<Value*> srcVector, String name = String());

		/// 创建一个复制值
		VCG_API OperationValue* createCopyValue(Type type, Value* src, String name = String());

		/// 创建一个复制值，类型也复制
		OperationValue* createCopyValue(Value* src, String name = String()) { return createCopyValue(src->getType(), src, name); }

		/// 创建一个条件选择值
		VCG_API OperationValue* createSelectValue(Type type, Value* condition, Value* trueBranch, Value* falseBranch, String name = String());

		/// 创建一个数组读操作
		VCG_API OperationValue* createArrayGetValue(Value* base, const std::vector<Value*>& index, String name = String());

		/// 创建一个数组写操作
		VCG_API OperationValue* createArraySetValue(Value* base, const std::vector<Value*>& index, Value* val, String name = String());

		/// 创建一个获取数组下界操作
		VCG_API OperationValue* createArrayLowerBoundValue(Value* base, Value* index, String name = String());

		/// 创建一个获取数组上界操作
		VCG_API OperationValue* createArrayUpperBoundValue(Value* base, Value* index, String name = String());

		/// 创建一个元组读操作
		VCG_API OperationValue* createTupleGetValue(Value* base, size_t index, String name = String());

		/// 创建一个元组写操作
		VCG_API OperationValue* createTupleSetValue(Value* base, size_t index, Value* val, String name = String());

		/// 创建一个结构体读操作
		VCG_API OperationValue* createStructGetValue(Value* base, String elem, String name = String());

		/// 创建一个结构体写操作
		VCG_API OperationValue* createStructSetValue(Value* base, String elem, Value* val, String name = String());
	};
};

/**
	* @brief 图片段生成器（抽象类）
	* @details
	* 用于对含有相同子结构的图片段进行压缩存储，也是表达无限图的方式。
	* 每个具体子类都对生成方式有特定的解释。
	*/
class SectionGenerator : public Section
{
protected:
	SectionGenerator(Context* context) : Section(context) {}
	DISABLE_COPY_MOVE(SectionGenerator)
		~SectionGenerator() override = default;

	void markReference() override;

public:
	VCG_API std::vector<VariableValue*> getVariableVector() const override;

	/// 获取所有模板形参
	const std::vector<InputValue*>& getTemplateVector() const { return templateVector; }

	/// 根据索引获取某个模板形参
	InputValue* getTemplate(size_t i) const { return templateVector[i]; }

	/// 获取模板形参数量
	size_t getTemplateVectorSize() const { return templateVector.size(); }

	/// 获取所有约束变元
	const std::vector<BoundValue*>& getBoundVector() const { return boundVector; }

	/// 根据索引获取某个约束变元
	BoundValue* getBound(size_t i) const { return boundVector[i]; }

	/// 获取约束变元数量
	size_t getBoundVectorSize() const { return boundVector.size(); }

	/// 获取所有分支合流值
	const std::vector<PhiValue*>& getPhiValueVector() const { return phiValueVector; }

	/// 根据索引获取某个分支合流值
	PhiValue* getPhiValue(size_t i) const { return phiValueVector[i]; }

	/// 获取分支合流值数量
	size_t getPhiValueVectorSize() const { return phiValueVector.size(); }

protected:
	std::vector<InputValue*> templateVector; //!< 图结构：模板形参，与序列化顺序保持一致
	std::vector<BoundValue*> boundVector; //!< 图结构：约束变元，与序列化顺序保持一致
	std::vector<PhiValue*> phiValueVector; //!< 图结构：phi函数，元素顺序不影响图结构，与序列化顺序保持一致

public:
	template<typename T>
	class BuilderTemplate : public Section::BuilderTemplate<T>
	{
	protected:
		/// 设置模板参数数量
		void setTemplateVectorSize(size_t n)
		{
			this->instance->templateVector.resize(n, nullptr);
		}

		/// 设置约束变元数量
		void setBoundVectorSize(size_t n)
		{
			this->instance->boundVector.resize(n, nullptr);
		}

	public:
		/// 根据索引设置某个模板参数
		void setTemplate(size_t i, InputValue::Builder& builder)
		{
			this->instance->templateVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
			builder.setTemplate(true);
		}

		/// 根据索引设置某个约束变元
		void setBound(size_t i, BoundValue::Builder& builder)
		{
			this->instance->boundVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
		}

		/// 添加一个分支合流值
		void addPhiValue(PhiValue::Builder& builder)
		{
			this->instance->phiValueVector.push_back(builder.getInstance());
			builder.setSection(this->instance);
			//builder.getInstance()->validate();
		}
	};

	class Builder : public BuilderTemplate<SectionGenerator>
	{
	public:
		DEFAULT_ALL(Builder)

			/// 由子类构造
			template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }

		/// 从基类建造者设置实例
		SectionGenerator* setInstance(const Section::Builder& src) { return instance = dynamic_cast<SectionGenerator*>(src.getInstance()); }
	};
};

/// 生成条件执行子结构
class BinaryBranch final : public SectionGenerator
{
protected:
	BinaryBranch(Context* context) : SectionGenerator(context) {}
	DISABLE_COPY_MOVE(BinaryBranch)
		~BinaryBranch() override = default;

public:
	class Builder : public SectionGenerator::BuilderTemplate<BinaryBranch>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API BinaryBranch* createInstance(Context* context);

		/// 从基类建造者设置实例
		BinaryBranch* setInstance(const Section::Builder& src) { return instance = dynamic_cast<BinaryBranch*>(src.getInstance()); }

		/// 创建一个分支合流值
		VCG_API PhiValue* createPhiValue(Type type, Value* ifValue, Value* elseValue, String name = String());

		/// 设置满足条件时的分支
		void setIfBranch(SectionCall::Builder& builder) { setSectionCall(0, builder); }

		/// 设置不满足条件时的分支
		void setElseBranch(SectionCall::Builder& builder) { setSectionCall(1, builder); }
	};

	class CallBuilder : public SectionCall::Builder
	{
	public:
		/// 设置条件操作数实参
		void setCondition(Value* value) { setSpecializer(0, value); }
	};

public:
	const char* getCategory() const override { return "BinaryBranch"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 获取条件操作数形参
	InputValue* getCondition() const { return getTemplate(0); }

	/// 获取满足条件时的分支
	SectionCall* getIfBranch() const { return getSectionCall(0); }

	/// 获取不满足条件时的分支
	SectionCall* getElseBranch() const { return getSectionCall(1); }
};

/// 生成次数可以确定的迭代循环子结构
class IterateLoop final : public SectionGenerator
{
protected:
	IterateLoop(Context* context) : SectionGenerator(context) {}
	~IterateLoop() override = default;

public:
	class Builder : public SectionGenerator::BuilderTemplate<IterateLoop>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API IterateLoop* createInstance(Context* context);

		/// 从基类建造者设置实例
		IterateLoop* setInstance(const Section::Builder& src) { return instance = dynamic_cast<IterateLoop*>(src.getInstance()); }

		/// 创建一个分支合流值
		VCG_API PhiValue* createPhiValue(Type type, Value* initValue, Value* loopValue, String name = String());

		/// 设置循环体对应的函数调用
		void setBodyCall(SectionCall::Builder& builder) { setSectionCall(0, builder); }
	};

	class CallBuilder : public SectionCall::Builder
	{
	public:
		/// 设置循环计数器开始下标实参
		void setBegin(Value* value) { setSpecializer(0, value); }

		/// 设置循环迭代次数实参
		void setTimes(Value* value) { setSpecializer(1, value); }

		/// 设置循环计数器步长实参
		void setStep(Value* value) { setSpecializer(2, value); }
	};

public:
	const char* getCategory() const override { return "IterateLoop"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 获取循环计数器开始下标形参
	InputValue* getBegin() const { return getTemplate(0); }

	/// 获取循环迭代次数形参
	InputValue* getTimes() const { return getTemplate(1); }

	/// 获取循环计数器步长形参
	InputValue* getStep() const { return getTemplate(2); }

	/// 获取循环计数器约束变元
	BoundValue* getCounter() const { return getBound(0); }

	/// 获取循环体对应的函数调用
	SectionCall* getBodyCall() const { return getSectionCall(0); }

	/// 获取循环体对应的图片段
	Section* getBody() const { return getBodyCall()->getCallee(); }
};

/// 生成对数组并行赋值的循环子结构
class ParallelLoop final : public SectionGenerator
{
protected:
	ParallelLoop(Context* context) : SectionGenerator(context) {}
	~ParallelLoop() override = default;

public:
	class Builder : public SectionGenerator::BuilderTemplate<ParallelLoop>
	{
	public:
		/// 在当前上下文创建一个新的实例
		VCG_API ParallelLoop* createInstance(Context* context, size_t counterNum);

		/// 从基类建造者设置实例
		ParallelLoop* setInstance(const Section::Builder& src) { return instance = dynamic_cast<ParallelLoop*>(src.getInstance()); }

		/// 创建一个分支合流值
		VCG_API PhiValue* createPhiValue(Type type, Value* initValue, Value* loopValue, String name = String());

		/// 设置循环体对应的函数调用
		void setBodyCall(SectionCall::Builder& builder) { setSectionCall(0, builder); }
	};

	class CallBuilder : public SectionCall::Builder {};

public:
	const char* getCategory() const override { return "ParallelLoop"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 获取循环计数器约束变元
	BoundValue* getCounter(size_t i) const { return getBound(i); }

	/// 获取循环计数器约束变元数量
	size_t getCounterNum() const { return getBoundVectorSize(); }

	/// 获取循环体对应的函数调用
	SectionCall* getBodyCall() const { return getSectionCall(0); }

	/// 获取循环体对应的图片段
	Section* getBody() const { return getBodyCall()->getCallee(); }
};
