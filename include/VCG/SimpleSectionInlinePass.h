#pragma once

#include "CopySectionPass.h"

/// 将输入打包成外部图片段的变换遍
class SimpleSectionInlinePass : virtual public Pass<GraphCallPath>, private TrivialSccAction
{
public:
	VCG_API SimpleSectionInlinePass();
	DEFAULT_COPY_MOVE(SimpleSectionInlinePass)

	/// 运行此类定义的操作，需要先调用load或setGraph方法，如果有分析遍则也要先运行
	VCG_API virtual void run();

	Value* mapValue(const ValueCallPath& src_path) const;

	Section* mapSection(const SectionCallPath& src_path) const;

	Value* mapValueByParent(Value* value) const;

	Section* mapSectionByParent(Section* section) const;

protected:
	/// 第一步：根据原图和分析遍结果，创建新Section中的Value对象的建造者
	virtual void createValueBuilders();

	/// 第二步：创建新Section对象的建造者
	virtual void createSectionBuilders();

	/// 第三步：根据建造者，为每个Value对象填充操作数
	virtual void createValueOperands();

	/// 第四步：生成结果
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
	GraphOuterSection result; //! 变换的结果

protected:
	std::vector<Value::Builder> valueBuilderVector;
	std::vector<Section::Builder> sectionBuilderVector;
	Value::Visitor<void, const std::vector<SectionCall*>&, uint32_t> createValueBuildersVisitor;
	Value::Visitor<void, const std::vector<SectionCall*>&, Value::Builder, Section::Builder> createValueOperandsVisitor;

private:
	std::unordered_map<SectionCallPath, SectionCall::Builder> sectionCallMap;
};

/// 简单图片段的内联（浅层次）
class SimpleSectionInlineShallowPass : public SimpleSectionInlinePass
{
public:
	VCG_API GraphOuterSection run(GraphOuterSection src);
};

/// 简单图片段的内联（深层次）
class SimpleSectionInlineDeepPass : public CopySectionDeepPass
{
public:
	VCG_API GraphValueProjection run(GraphValueProjection src);

protected:
	VCG_API void runOnSubgraph(uint32_t i) override;
};
