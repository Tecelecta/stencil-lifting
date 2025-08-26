#pragma once

#include "PassBase.h"

/// 
class CopySectionShallowPass : virtual public Pass<GraphOuterSection>, public TransformResult
{
public:
	VCG_API CopySectionShallowPass();
	DEFAULT_COPY_MOVE(CopySectionShallowPass)

	/// loadsetGraph
	VCG_API virtual void run();

	/// 
	VCG_API virtual GraphOuterSection run(GraphOuterSection src);

	Value* mapValue(Value* value) const override;

	Section* mapSection(Section* section) const override;

	Value* mapValueByParent(Value* value) const;

	Section* mapSectionByParent(Section* section) const;

protected:
	/// SectionValue
	virtual void createValueBuilders();

	/// Section
	virtual void createSectionBuilders();

	/// Value
	virtual void createValueOperands();

	/// 
	virtual void createResult();

	SectionCall::Builder getSectionCallBuilder(SectionCall* src_call, Section::Builder sectionBuilder);

	SectionCall::Builder initSectionCallBuilder(SectionCall* src_call);

public:
	GraphOuterSection result; //! 

protected:
	std::vector<Value::Builder> valueBuilderVector;
	std::vector<Section::Builder> sectionBuilderVector;
	Value::Visitor<void, uint32_t> createValueBuildersVisitor;
	Value::Visitor<void, Value::Builder, Section::Builder> createValueOperandsVisitor;
	std::unordered_map<SectionCall*, SectionCall::Builder> sectionCallMap;
};

/// 
class CopySectionDeepPass : virtual public Pass<GraphValueProjection>, public TransformResult
{
public:
	DEFAULT_ALL(CopySectionDeepPass)

	/// Section
	VCG_API virtual void run();

	/// 
	VCG_API virtual GraphValueProjection run(GraphValueProjection src);

	VCG_API Value* mapValue(Value* value) const override;

	VCG_API Section* mapSection(Section* section) const override;

protected:
	VCG_API virtual void runOnSubgraph(uint32_t i);

	void updateResultVector(const CopySectionShallowPass& subPass);

public:
	GraphValueProjection result; //! 
};

/// 
class CopyLoopAndBodyPass : virtual public Pass<GraphLoopAndBody>, public TransformResult
{
public:
	DEFAULT_ALL(CopyLoopAndBodyPass)

	/// Section
	VCG_API virtual void run();

	/// 
	VCG_API virtual GraphValueProjection run(GraphLoopAndBody src);

	VCG_API void clearResult();

	VCG_API Value* mapValue(Value* value) const override;

	VCG_API Section* mapSection(Section* section) const override;

	Value* mapValueByParent(Value* value) const;

	Section* mapSectionByParent(Section* section) const;

protected:
	VCG_API virtual void runOnBody();

	VCG_API virtual void runOnLoop();

	void updateResultVector(const CopySectionShallowPass& subPass);

public:
	GraphValueProjection result; //! 
};
