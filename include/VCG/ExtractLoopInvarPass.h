#pragma once

#include "CopySectionPass.h"

class ExtractLoopInvarShallowPass : virtual public Pass<GraphLoopAndBody>,
	public TransformResult, private SccAction
{
public:
	VCG_API ExtractLoopInvarShallowPass();
	DEFAULT_COPY_MOVE(ExtractLoopInvarShallowPass)

	VCG_API void run();

	Value* mapValue(Value* value) const override;

	Section* mapSection(Section* section) const override;

	Value* mapValueByParent(Value* value) const;

	Section* mapSectionByParent(Section* section) const;

private:
	void createValueBuilders();

	void createSectionBuilders();

	void createValueOperands();

	void createValueOperands(uint32_t i, Section::Builder sectionBuilder, uint32_t layer);

	void createBodyCall();

	void createLoopCall();

	SectionCall::Builder getSectionCallBuilder(SectionCall* src_call, Section::Builder sectionBuilder);

	SectionCall::Builder initSectionCallBuilder(SectionCall* src_call);

	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

	bool isInvar(uint32_t i) const;

public:
	GraphValueProjection result; //! 

private:
	Value::Visitor<void, uint32_t> createValueBuildersVisitor;
	Value::Visitor<void, Value::Builder, Section::Builder, uint32_t> createValueOperandsVisitor;
	std::unordered_map<SectionCall*, SectionCall::Builder> sectionCallMap;
	std::vector<uint32_t> valueLayerVector;
	std::vector<std::array<Value::Builder, 3>> valueBuilderEachLayerVector;
	std::vector<std::array<Value*, 3>> valueResultEachLayerVector;
	SimpleSection::Builder bodyBuilder; // layer 0
	SectionGenerator::Builder loopBuilder; // layer 1
	SimpleSection::Builder outerBuilder; // layer 2
	SectionCall::Builder callBodyBuilder;
	SectionCall::Builder callLoopBuilder;
};

class ExtractLoopInvarDeepPass : public CopySectionDeepPass
{
protected:
	VCG_API void runOnSubgraph(uint32_t i) override;

private:
	void runOnLoop(SectionGenerator* loop);
};
