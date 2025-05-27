#pragma once

#include "PassBase.h"

/// 拷贝一个计算图（浅层次）
class CopySectionShallowPass : virtual public Pass<GraphOuterSection>, public TransformResult
{
public:
	VCG_API CopySectionShallowPass();
	DEFAULT_COPY_MOVE(CopySectionShallowPass)

	/// 运行此类定义的操作，需要先调用load或setGraph方法，如果有分析遍则也要先运行
	VCG_API virtual void run();

	/// 快捷运行
	VCG_API virtual GraphOuterSection run(GraphOuterSection src);

	Value* mapValue(Value* value) const override;

	Section* mapSection(Section* section) const override;

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

	SectionCall::Builder getSectionCallBuilder(SectionCall* src_call, Section::Builder sectionBuilder);

	SectionCall::Builder initSectionCallBuilder(SectionCall* src_call);

public:
	GraphOuterSection result; //! 变换的结果

protected:
	std::vector<Value::Builder> valueBuilderVector;
	std::vector<Section::Builder> sectionBuilderVector;
	Value::Visitor<void, uint32_t> createValueBuildersVisitor;
	Value::Visitor<void, Value::Builder, Section::Builder> createValueOperandsVisitor;
	std::unordered_map<SectionCall*, SectionCall::Builder> sectionCallMap;
};

/// 拷贝一个计算图（深层次）
class CopySectionDeepPass : virtual public Pass<GraphValueProjection>, public TransformResult
{
public:
	DEFAULT_ALL(CopySectionDeepPass)

	/// 自内而外遍历每个Section
	VCG_API virtual void run();

	/// 快捷运行
	VCG_API virtual GraphValueProjection run(GraphValueProjection src);

	VCG_API Value* mapValue(Value* value) const override;

	VCG_API Section* mapSection(Section* section) const override;

protected:
	VCG_API virtual void runOnSubgraph(uint32_t i);

	void updateResultVector(const CopySectionShallowPass& subPass);

public:
	GraphValueProjection result; //! 变换的结果
};

/// 拷贝一个循环和循环体
class CopyLoopAndBodyPass : virtual public Pass<GraphLoopAndBody>, public TransformResult
{
public:
	DEFAULT_ALL(CopyLoopAndBodyPass)

	/// 自内而外遍历每个Section
	VCG_API virtual void run();

	/// 快捷运行
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
	GraphValueProjection result; //! 变换的结果
};
