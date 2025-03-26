#define VCG_EXPORTS

#include "SimpleSectionInlinePass.h"
#include "CopySectionPass.h"

SimpleSectionInlinePass::SimpleSectionInlinePass()
{
	createValueBuildersVisitor.defaultFunction = [this](Value* value, const std::vector<SectionCall*>& callVector, uint32_t i)
		{
			valueResultVector[i] = value;
		};

	createValueBuildersVisitor.visitVariableValue = [this](VariableValue* value, const std::vector<SectionCall*>& callVector, uint32_t i)
		{
			valueBuilderVector[i] = value->cloneBuilder();
			valueResultVector[i] = valueBuilderVector[i].getInstance();
		};

	createValueBuildersVisitor.visitInputValue = [this](InputValue* value, const std::vector<SectionCall*>& callVector, uint32_t i)
		{
			if (callVector.empty())
			{
				createValueBuildersVisitor.visitVariableValue(value, callVector, i);
			}
			else
			{
				// 将形参映射为实参
				valueResultVector[i] = mapValue({ callVector.back()->getArgument(value->getIndex()),
					std::vector<SectionCall*>(callVector.begin(), callVector.end() - 1) });
			}
		};

	createValueBuildersVisitor.visitResultValue = [this](ResultValue* value, const std::vector<SectionCall*>& callVector, uint32_t i)
		{
			ValueCallPath src_path = { value->getSrc(), callVector };
			src_path.callVector.push_back(value->getCall());
			if (vertexExists(src_path))
			{
				// 将结果映射为数据源
				valueResultVector[i] = mapValue({ src_path.value, src_path.callVector });
			}
			else
			{
				auto src = mapValueByParent(value->getSrc());
				if (dynamic_cast<VariableValue*>(src) == nullptr)
				{
					// 结果值被映射为常量
					valueResultVector[i] = src;
				}
				else
				{
					createValueBuildersVisitor.visitVariableValue(value, callVector, i);
				}
			}
		};

	createValueOperandsVisitor.defaultFunction = [](Value* value, const std::vector<SectionCall*>& callVector,
		Value::Builder valueBuilder, Section::Builder sectionBuilder) {};

	createValueOperandsVisitor.visitInputValue = [this](InputValue* value, const std::vector<SectionCall*>& callVector,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			assert(!value->isTemplate());
			InputValue::Builder builder;
			builder.setInstance(valueBuilder);
			sectionBuilder.addParameter(builder);
		};

	createValueOperandsVisitor.visitResultValue = [this](ResultValue* value, const std::vector<SectionCall*>& callVector,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			auto retvar = dynamic_cast<VariableValue*>(mapValueByParent(value->getSrc()));
			assert(retvar != nullptr);
			ResultValue::Builder builder;
			builder.setInstance(valueBuilder);
			builder.setSrc(retvar);
			auto src_call = value->getCall();
			SectionCallPath src_path = { src_call->getCallee(), callVector };
			src_path.callVector.push_back(src_call);
			auto callBuilder = getSectionCallBuilder(src_path, sectionBuilder);
			assert(retvar->getSection() == callBuilder.getInstance()->getCallee());
			callBuilder.addResultValue(builder);
		};

	createValueOperandsVisitor.visitOperationValue = [this](OperationValue* value, const std::vector<SectionCall*>& callVector,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			OperationValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getSrcVectorSize(); i++)
			{
				builder.setSrc(i, mapValue({ value->getSrc(i), callVector }));
			}
			SimpleSection::Builder simpleSectionBuilder;
			simpleSectionBuilder.setInstance(sectionBuilder);
			simpleSectionBuilder.addOperationValue(builder);
		};

	createValueOperandsVisitor.visitPhiValue = [this](PhiValue* value, const std::vector<SectionCall*>& callVector,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			PhiValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getIncomingVectorSize(); i++)
			{
				builder.setIncoming(i, mapValue({ value->getIncoming(i), callVector }));
			}
			SectionGenerator::Builder sectionGeneratorBuilder;
			sectionGeneratorBuilder.setInstance(sectionBuilder);
			sectionGeneratorBuilder.addPhiValue(builder);
		};
}

void SimpleSectionInlinePass::run()
{
	createValueBuilders();
	createSectionBuilders();
	createValueOperands();
	createResult();
}

Value* SimpleSectionInlinePass::mapValue(const ValueCallPath& src_path) const
{
	if (dynamic_cast<VariableValue*>(src_path.value) != nullptr)
	{
		return valueResultVector[vertexId(src_path)];
	}
	else
	{
		return valueResultVector[vertexId({ src_path.value, {} })];
	}
}

Section* SimpleSectionInlinePass::mapSection(const SectionCallPath& src_path) const
{
	return sectionResultVector[subgraphId(src_path)];
}

Value* SimpleSectionInlinePass::mapValueByParent(Value* value) const
{
	return parent != nullptr ? parent->mapValue(value) : value;
}

Section* SimpleSectionInlinePass::mapSectionByParent(Section* section) const
{
	return parent != nullptr ? parent->mapSection(section) : section;
}

void SimpleSectionInlinePass::createSectionBuilders()
{
	sectionResultVector.resize(getSubgraphNum(), nullptr);
	sectionBuilderVector.resize(getSubgraphNum());
	for (auto section : getRootSections())
	{
		assert(dynamic_cast<SimpleSection*>(section) != nullptr);
		auto i = subgraphId({ section, {} });
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			auto& sectionBuilder = sectionBuilderVector[i];
			sectionBuilder = section->initBuilder();
			sectionBuilder.copySectionName(section);
			sectionResultVector[i] = sectionBuilder.getInstance();
		}
	}
}

void SimpleSectionInlinePass::createValueBuilders()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	valueBuilderVector.resize(getVertexNum());
	iterateSCC(*this);
}

void SimpleSectionInlinePass::createValueOperands()
{
	for (uint32_t i = 0; i < valueBuilderVector.size(); i++)
	{
		auto& builder = valueBuilderVector[i];
		if (auto variableValue = dynamic_cast<VariableValue*>(builder.getInstance()))
		{
			const auto& callVector = vertexAt(i).callVector;
			uint32_t id;
			if (callVector.empty())
			{
				id = getSubgraphByVertex(i);
			}
			else
			{
				id = subgraphId({ callVector.front()->getCaller(), {} });
			}
			createValueOperandsVisitor(vertexAt(i).value, callVector, builder, sectionBuilderVector[id]);
		}
	}
}

void SimpleSectionInlinePass::createResult()
{
	std::vector<Section*> dstSectionVector;
	for (size_t i = 0; i < getSubgraphNum(); i++)
	{
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			if (sectionResultVector[i] != nullptr)
			{
				dstSectionVector.push_back(sectionResultVector[i]);
			}
		}
	}
	result.load(dstSectionVector);
}

SectionCall::Builder SimpleSectionInlinePass::getSectionCallBuilder(const SectionCallPath& src_path, Section::Builder sectionBuilder)
{
	auto iter = sectionCallMap.find(src_path);
	if (iter == sectionCallMap.end())
	{
		auto builder = initSectionCallBuilder(src_path);
		sectionBuilder.addSectionCall(builder);
		iter = sectionCallMap.emplace(src_path, builder).first;
	}
	return iter->second;
}

SectionCall::Builder SimpleSectionInlinePass::initSectionCallBuilder(const SectionCallPath& src_path)
{
	SectionCall::Builder builder;
	auto src_call = src_path.callVector.back();
	auto src_callee = src_path.section;
	auto dst_callee = mapSectionByParent(src_callee);
	builder.createInstance(dst_callee);
	std::vector<SectionCall*> arg_callVector(src_path.callVector.begin(), src_path.callVector.end() - 1);
	for (auto src_param : src_callee->getParameterVector())
	{
		if (auto dst_param = dynamic_cast<InputValue*>(mapValueByParent(src_param)))
		{
			if (dst_param->getSection() == dst_callee)
			{
				auto src_arg = src_call->getArgument(src_param->getIndex());
				auto dst_arg = mapValue({ src_arg, arg_callVector });
				builder.setArgument(dst_param->getIndex(), dst_arg);
			}
		}
	}
	for (size_t i = 0; i < src_call->getSpecializerVectorSize(); i++)
	{
		builder.setSpecializer(i, mapValue({ src_call->getSpecializer(i), arg_callVector }));
	}
	return builder;
}

bool SimpleSectionInlinePass::runOnTrivial(uint32_t i)
{
	if (vertexFilter.empty() || vertexFilter.at(i))
	{
		createValueBuildersVisitor(vertexAt(i).value, vertexAt(i).callVector, i);
	}
	return true;
}

GraphOuterSection SimpleSectionInlineShallowPass::run(GraphOuterSection src)
{
	load(src.getRootSections());
	SimpleSectionInlinePass::run();
	return std::move(result);
}

GraphValueProjection SimpleSectionInlineDeepPass::run(GraphValueProjection src)
{
	setGraph(std::move(src));
	CopySectionDeepPass::run();
	return std::move(result);
}

void SimpleSectionInlineDeepPass::runOnSubgraph(uint32_t i)
{
	if (auto simpleSection = dynamic_cast<SimpleSection*>(subgraphAt(i)))
	{
		simpleSection->validate();
		SimpleSectionInlinePass subPass;
		subPass.parent = this;
		subPass.load({ simpleSection });
		subPass.run();
		sectionResultVector[i] = subPass.result.getRootSections()[0];
		for (uint32_t j = 0; j < subPass.getVertexNum(); j++)
		{
			auto path = subPass.vertexAt(j);
			valueResultVector[vertexId(path.value)] = subPass.mapValue(path);
		}
	}
	else
	{
		CopySectionDeepPass::runOnSubgraph(i);
	}
}
