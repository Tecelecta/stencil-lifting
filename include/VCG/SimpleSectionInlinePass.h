#pragma once

#include "CopySectionPass.h"

/// 
class SimpleSectionInlinePass : virtual public Pass<GraphCallPath>, private TrivialSccAction
{
public:
	VCG_API SimpleSectionInlinePass();
	DEFAULT_COPY_MOVE(SimpleSectionInlinePass)

	/// loadsetGraph
	VCG_API virtual void run();

	Value* mapValue(const ValueCallPath& src_path) const;

	Section* mapSection(const SectionCallPath& src_path) const;

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

	bool expandSection(Section* section) const override
	{
		return dynamic_cast<SimpleSection*>(section) != nullptr;
	}

private:
	SectionCall::Builder getSectionCallBuilder(const SectionCallPath& src_path, Section::Builder sectionBuilder);

	SectionCall::Builder initSectionCallBuilder(const SectionCallPath& src_path);

	bool runOnTrivial(uint32_t i) override;

public:
	const class CopySectionDeepPass* parent = nullptr;
	std::vector<Value*> valueResultVector;
	std::vector<Section*> sectionResultVector;
	GraphOuterSection result; //! 

protected:
	std::vector<Value::Builder> valueBuilderVector;
	std::vector<Section::Builder> sectionBuilderVector;
	Value::Visitor<void, const std::vector<SectionCall*>&, uint32_t> createValueBuildersVisitor;
	Value::Visitor<void, const std::vector<SectionCall*>&, Value::Builder, Section::Builder> createValueOperandsVisitor;

private:
	std::unordered_map<SectionCallPath, SectionCall::Builder> sectionCallMap;
};

/// 
class SimpleSectionInlineShallowPass : public SimpleSectionInlinePass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src);
};

/// 
class SimpleSectionInlineDeepPass : public CopySectionDeepPass
{
public:
	VCG_API GraphValueProjection run(GraphValueProjection src);

protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
