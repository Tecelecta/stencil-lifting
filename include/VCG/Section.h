#pragma once

/**
 * @file Section.h
 * @brief 
 */

#include "Value.h"

/// 
class SectionCall : public MarkedObject
{
protected:
	explicit SectionCall(Context* context) : MarkedObject(context) {}
	DEFAULT_COPY(SectionCall)
	~SectionCall() override = default;

	void markReference() override;

public:
	/// 
	Section* getCaller() const { return caller; }

	/// 
	Section* getCallee() const { return callee; }

	/// 
	size_t getIndex() const { return index; }

	/// 
	const std::vector<Value*>& getArgumentVector() const { return argumentVector; }

	/// 
	Value* getArgument(size_t i) const { return argumentVector[i]; }

	/// 
	size_t getArgumentVectorSize() const { return argumentVector.size(); }

	/// 
	const std::vector<Value*>& getSpecializerVector() const { return specializerVector; }

	/// 
	Value* getSpecializer(size_t i) const { return specializerVector[i]; }

	/// 
	size_t getSpecializerVectorSize() const { return specializerVector.size(); }

	/// 
	const std::vector<ResultValue*>& getResultValueVector() const { return resultValueVector; }

	/// 
	ResultValue* getResultValue(size_t i) const { return resultValueVector[i]; }

	/// 
	size_t getResultValueVectorSize() const { return resultValueVector.size(); }

	/// 
	VCG_API void validate() const;

private:
	Section* caller = nullptr; //!< 
	Section* callee = nullptr; //!< 
	size_t index = 0; //!< 
	std::vector<Value*> argumentVector; /// parameter
	std::vector<Value*> specializerVector; /// template
	std::vector<ResultValue*> resultValueVector; //!< 

public:
	class Builder : public BuilderBase<SectionCall>
	{
	public:
		/**
			* @brief 
			* @details
			* argumentVectorspecializerVector
			*/
		VCG_API SectionCall* createInstance(Section* callee);

		/// 
		VCG_API SectionCall* cloneInstance(SectionCall* src);

		/**
			* @brief 
			* @details
			* 
			* Section
			*/
		void setCaller(Section* caller) { this->instance->caller = caller; }

		/// 
		void setIndex(size_t index) { this->instance->index = index; }

		/// 
		void setArgument(size_t i, Value* value) { this->instance->argumentVector[i] = value; }

		/// 
		void setSpecializer(size_t i, Value* value) { this->instance->specializerVector[i] = value; }

		/// 
		VCG_API void addResultValue(ResultValue::Builder& builder);

		/// 
		VCG_API ResultValue* createResultValue(VariableValue* src, String name = String());
	};
};

/**
	* @brief 
	* @details
	* 
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
		/// 
		void addParameter(InputValue::Builder& builder)
		{
			builder.setIndex((uint32_t)this->instance->parameterVector.size());
			this->instance->parameterVector.push_back(builder.getInstance());
			builder.setSection(this->instance);
			builder.setTemplate(false);
		}

		/// 
		void setParameterVectorSize(size_t n)
		{
			this->instance->parameterVector.resize(n, nullptr);
		}

		/// 
		void setParameter(uint32_t i, InputValue::Builder& builder)
		{
			this->instance->parameterVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
			builder.setTemplate(false);
		}

		/// 
		InputValue* createParameter(Type type, String name = String())
		{
			InputValue::Builder builder;
			builder.createInstance(this->instance->getContext());
			builder.setType(type);
			builder.setName(name);
			addParameter(builder);
			return builder.getInstance();
		}

		/// 
		InputValue* createParameter(Type type, std::string_view name)
		{
			return createParameter(type, this->instance->getContext()->getString(name));
		}

		/// 
		void setSectionCall(size_t i, SectionCall::Builder& builder)
		{
			this->instance->sectionCallVector[i] = builder.getInstance();
			builder.setIndex(i);
			builder.setCaller(this->instance);
		}

		/// 
		void setSectionCallVectorSize(size_t n) { this->instance->sectionCallVector.resize(n, nullptr); }

		/// 
		void addSectionCall(SectionCall::Builder& builder)
		{
			builder.setIndex(this->instance->sectionCallVector.size());
			this->instance->sectionCallVector.push_back(builder.getInstance());
			builder.setCaller(this->instance);
		}

		/// 
		void setSectionName(String name) { this->instance->sectionName = name == nullptr ? this->instance->getContext()->getEmptyString() : name; }

		/// 
		void setSectionName(std::string_view name) { this->instance->sectionName = this->instance->getContext()->getString(name); }

		/// 
		void copySectionName(Section* src) { this->instance->sectionName = src->getName(); }
	};

	class Builder : public BuilderTemplate<Section>
	{
	public:
		DEFAULT_ALL(Builder)

		/// 
		template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }
	};

public:
	/// 
	const std::vector<InputValue*>& getParameterVector() const { return parameterVector; }

	/// 
	InputValue* getParameter(size_t i) const { return parameterVector[i]; }

	/// 
	size_t getParameterVectorSize() const { return parameterVector.size(); }

	/// 
	const std::vector<SectionCall*>& getSectionCallVector() const { return sectionCallVector; }

	/// 
	SectionCall* getSectionCall(size_t i) const { return sectionCallVector[i]; }

	/// 
	size_t getSectionCallVectorSize() const { return sectionCallVector.size(); }

	/// 
	String getName() const { return sectionName; }

	/// 
	virtual VCG_API std::vector<VariableValue*> getVariableVector() const;

	/// 
	virtual const char* getCategory() const = 0;

	/// 
	virtual Builder initBuilder() const = 0;

	/// 
	VCG_API void validate() const;

protected:
	std::vector<InputValue*> parameterVector; //!< 
	std::vector<SectionCall*> sectionCallVector; //!< 
	String sectionName; //!< Section
};

/// 
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

	/// 
	const std::vector<OperationValue*>& getOperationValueVector() const { return operationValueVector; }

	/// 
	OperationValue* getOperationValue(size_t i) const { return operationValueVector[i]; }

	/// 
	size_t getOperationValueVectorSize() const { return operationValueVector.size(); }

	/// 
	const char* getCategory() const override { return "SimpleSection"; }

private:
	std::vector<OperationValue*> operationValueVector; //!< 

public:
	class Builder : public Section::BuilderTemplate<SimpleSection>
	{
	public:
		/// 
		VCG_API SimpleSection* createInstance(Context* context);

		/// 
		SimpleSection* setInstance(const Section::Builder& src) { return instance = dynamic_cast<SimpleSection*>(src.getInstance()); }

		/// 
		void addOperationValue(OperationValue::Builder& builder)
		{
			instance->operationValueVector.push_back(builder.getInstance());
			builder.setSection(this->instance);
			//builder.getInstance()->validate();
		}

		/// 
		VCG_API OperationValue* createOperationValue(Operation op, std::vector<Value*> srcVector, String name = String());

		/// 
		VCG_API OperationValue* createCopyValue(Type type, Value* src, String name = String());

		/// 
		OperationValue* createCopyValue(Value* src, String name = String()) { return createCopyValue(src->getType(), src, name); }

		/// 
		VCG_API OperationValue* createSelectValue(Type type, Value* condition, Value* trueBranch, Value* falseBranch, String name = String());

		/// 
		VCG_API OperationValue* createArrayGetValue(Value* base, const std::vector<Value*>& index, String name = String());

		/// 
		VCG_API OperationValue* createArraySetValue(Value* base, const std::vector<Value*>& index, Value* val, String name = String());

		/// 
		VCG_API OperationValue* createArrayLowerBoundValue(Value* base, Value* index, String name = String());

		/// 
		VCG_API OperationValue* createArrayUpperBoundValue(Value* base, Value* index, String name = String());

		/// 
		VCG_API OperationValue* createTupleGetValue(Value* base, size_t index, String name = String());

		/// 
		VCG_API OperationValue* createTupleSetValue(Value* base, size_t index, Value* val, String name = String());

		/// 
		VCG_API OperationValue* createStructGetValue(Value* base, String elem, String name = String());

		/// 
		VCG_API OperationValue* createStructSetValue(Value* base, String elem, Value* val, String name = String());
	};
};

/**
	* @brief 
	* @details
	* 
	* 
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

	/// 
	const std::vector<InputValue*>& getTemplateVector() const { return templateVector; }

	/// 
	InputValue* getTemplate(size_t i) const { return templateVector[i]; }

	/// 
	size_t getTemplateVectorSize() const { return templateVector.size(); }

	/// 
	const std::vector<BoundValue*>& getBoundVector() const { return boundVector; }

	/// 
	BoundValue* getBound(size_t i) const { return boundVector[i]; }

	/// 
	size_t getBoundVectorSize() const { return boundVector.size(); }

	/// 
	const std::vector<PhiValue*>& getPhiValueVector() const { return phiValueVector; }

	/// 
	PhiValue* getPhiValue(size_t i) const { return phiValueVector[i]; }

	/// 
	size_t getPhiValueVectorSize() const { return phiValueVector.size(); }

protected:
	std::vector<InputValue*> templateVector; //!< 
	std::vector<BoundValue*> boundVector; //!< 
	std::vector<PhiValue*> phiValueVector; //!< phi

public:
	template<typename T>
	class BuilderTemplate : public Section::BuilderTemplate<T>
	{
	protected:
		/// 
		void setTemplateVectorSize(size_t n)
		{
			this->instance->templateVector.resize(n, nullptr);
		}

		/// 
		void setBoundVectorSize(size_t n)
		{
			this->instance->boundVector.resize(n, nullptr);
		}

	public:
		/// 
		void setTemplate(size_t i, InputValue::Builder& builder)
		{
			this->instance->templateVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
			builder.setTemplate(true);
		}

		/// 
		void setBound(size_t i, BoundValue::Builder& builder)
		{
			this->instance->boundVector[i] = builder.getInstance();
			builder.setSection(this->instance);
			builder.setIndex(i);
		}

		/// 
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

			/// 
			template<typename T>
		Builder(const BuilderTemplate<T>& src) { instance = src.getInstance(); }

		/// 
		SectionGenerator* setInstance(const Section::Builder& src) { return instance = dynamic_cast<SectionGenerator*>(src.getInstance()); }
	};
};

/// 
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
		/// 
		VCG_API BinaryBranch* createInstance(Context* context);

		/// 
		BinaryBranch* setInstance(const Section::Builder& src) { return instance = dynamic_cast<BinaryBranch*>(src.getInstance()); }

		/// 
		VCG_API PhiValue* createPhiValue(Type type, Value* ifValue, Value* elseValue, String name = String());

		/// 
		void setIfBranch(SectionCall::Builder& builder) { setSectionCall(0, builder); }

		/// 
		void setElseBranch(SectionCall::Builder& builder) { setSectionCall(1, builder); }
	};

	class CallBuilder : public SectionCall::Builder
	{
	public:
		/// 
		void setCondition(Value* value) { setSpecializer(0, value); }
	};

public:
	const char* getCategory() const override { return "BinaryBranch"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 
	InputValue* getCondition() const { return getTemplate(0); }

	/// 
	SectionCall* getIfBranch() const { return getSectionCall(0); }

	/// 
	SectionCall* getElseBranch() const { return getSectionCall(1); }
};

/// 
class IterateLoop final : public SectionGenerator
{
protected:
	IterateLoop(Context* context) : SectionGenerator(context) {}
	~IterateLoop() override = default;

public:
	class Builder : public SectionGenerator::BuilderTemplate<IterateLoop>
	{
	public:
		/// 
		VCG_API IterateLoop* createInstance(Context* context);

		/// 
		IterateLoop* setInstance(const Section::Builder& src) { return instance = dynamic_cast<IterateLoop*>(src.getInstance()); }

		/// 
		VCG_API PhiValue* createPhiValue(Type type, Value* initValue, Value* loopValue, String name = String());

		/// 
		void setBodyCall(SectionCall::Builder& builder) { setSectionCall(0, builder); }
	};

	class CallBuilder : public SectionCall::Builder
	{
	public:
		/// 
		void setBegin(Value* value) { setSpecializer(0, value); }

		/// 
		void setTimes(Value* value) { setSpecializer(1, value); }

		/// 
		void setStep(Value* value) { setSpecializer(2, value); }
	};

public:
	const char* getCategory() const override { return "IterateLoop"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 
	InputValue* getBegin() const { return getTemplate(0); }

	/// 
	InputValue* getTimes() const { return getTemplate(1); }

	/// 
	InputValue* getStep() const { return getTemplate(2); }

	/// 
	BoundValue* getCounter() const { return getBound(0); }

	/// 
	SectionCall* getBodyCall() const { return getSectionCall(0); }

	/// 
	Section* getBody() const { return getBodyCall()->getCallee(); }
};

/// 
class ParallelLoop final : public SectionGenerator
{
protected:
	ParallelLoop(Context* context) : SectionGenerator(context) {}
	~ParallelLoop() override = default;

public:
	class Builder : public SectionGenerator::BuilderTemplate<ParallelLoop>
	{
	public:
		/// 
		VCG_API ParallelLoop* createInstance(Context* context, size_t counterNum);

		/// 
		ParallelLoop* setInstance(const Section::Builder& src) { return instance = dynamic_cast<ParallelLoop*>(src.getInstance()); }

		/// 
		VCG_API PhiValue* createPhiValue(Type type, Value* initValue, Value* loopValue, String name = String());

		/// 
		void setBodyCall(SectionCall::Builder& builder) { setSectionCall(0, builder); }
	};

	class CallBuilder : public SectionCall::Builder {};

public:
	const char* getCategory() const override { return "ParallelLoop"; }

	VCG_API Section::Builder initBuilder() const override;

	/// 
	BoundValue* getCounter(size_t i) const { return getBound(i); }

	/// 
	size_t getCounterNum() const { return getBoundVectorSize(); }

	/// 
	SectionCall* getBodyCall() const { return getSectionCall(0); }

	/// 
	Section* getBody() const { return getBodyCall()->getCallee(); }
};
