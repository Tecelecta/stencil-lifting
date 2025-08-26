#define VCG_EXPORTS

#include "CopySectionPass.h"

CopySectionShallowPass::CopySectionShallowPass()
{
	createValueBuildersVisitor.defaultFunction = [this](Value* value, uint32_t i)
		{
			valueResultVector[i] = value;
		};

	createValueBuildersVisitor.visitVariableValue = [this](VariableValue* value, uint32_t i)
		{
			valueBuilderVector[i] = value->cloneBuilder();
			valueResultVector[i] = valueBuilderVector[i].getInstance();
		};

	createValueBuildersVisitor.visitResultValue = [this](ResultValue* value, uint32_t i)
		{
			auto src = mapValueByParent(value->getSrc());
			if (dynamic_cast<VariableValue*>(src) == nullptr)
			{
				valueResultVector[i] = src;
			}
			else
			{
				createValueBuildersVisitor.visitVariableValue(value, i);
			}
		};

	createValueOperandsVisitor.defaultFunction = [](Value* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder) {};

	createValueOperandsVisitor.visitInputValue = [this](InputValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			InputValue::Builder builder;
			builder.setInstance(valueBuilder);
			if (value->isTemplate())
			{
				SectionGenerator::Builder sectionGeneratorBuilder;
				sectionGeneratorBuilder.setInstance(sectionBuilder);
				sectionGeneratorBuilder.setTemplate(value->getIndex(), builder);
			}
			else
			{
				sectionBuilder.addParameter(builder);
			}
		};

	createValueOperandsVisitor.visitBoundValue = [this](BoundValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			BoundValue::Builder builder;
			builder.setInstance(valueBuilder);
			SectionGenerator::Builder sectionGeneratorBuilder;
			sectionGeneratorBuilder.setInstance(sectionBuilder);
			sectionGeneratorBuilder.setBound(value->getIndex(), builder);
		};

	createValueOperandsVisitor.visitResultValue = [this](ResultValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			auto retvar = dynamic_cast<VariableValue*>(mapValueByParent(value->getSrc()));
			assert(retvar != nullptr);
			ResultValue::Builder builder;
			builder.setInstance(valueBuilder);
			builder.setSrc(retvar);
			auto callBuilder = getSectionCallBuilder(value->getCall(), sectionBuilder);
			assert(retvar->getSection() == callBuilder.getInstance()->getCallee());
			callBuilder.addResultValue(builder);
		};

	createValueOperandsVisitor.visitOperationValue = [this](OperationValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			OperationValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getSrcVectorSize(); i++)
			{
				builder.setSrc(i, mapValue(value->getSrc(i)));
			}
			SimpleSection::Builder simpleSectionBuilder;
			simpleSectionBuilder.setInstance(sectionBuilder);
			simpleSectionBuilder.addOperationValue(builder);
		};

	createValueOperandsVisitor.visitPhiValue = [this](PhiValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			PhiValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getIncomingVectorSize(); i++)
			{
				builder.setIncoming(i, mapValue(value->getIncoming(i)));
			}
			SectionGenerator::Builder sectionGeneratorBuilder;
			sectionGeneratorBuilder.setInstance(sectionBuilder);
			sectionGeneratorBuilder.addPhiValue(builder);
		};
}

void CopySectionShallowPass::run()
{
	createValueBuilders();
	createSectionBuilders();
	createValueOperands();
	createResult();
}

GraphOuterSection CopySectionShallowPass::run(GraphOuterSection src)
{
	setGraph(std::move(src));
	run();
	return std::move(result);
}

Value* CopySectionShallowPass::mapValue(Value* value) const
{
	return vertexExists(value) ? valueResultVector[vertexId(value)] : value;
}

Section* CopySectionShallowPass::mapSection(Section* section) const
{
	return subgraphExists(section) ? sectionResultVector[subgraphId(section)] : section;
}

Value* CopySectionShallowPass::mapValueByParent(Value* value) const
{
	return parent != nullptr ? parent->mapValue(value) : value;
}

Section* CopySectionShallowPass::mapSectionByParent(Section* section) const
{
	return parent != nullptr ? parent->mapSection(section) : section;
}

void CopySectionShallowPass::createValueBuilders()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	valueBuilderVector.resize(getVertexNum());
	std::vector<uint32_t> noRepresentative;
	for (uint32_t i = 0; i < getVertexNum(); i++)
	{
		if (vertexFilter.empty() || vertexFilter.at(i))
		{
			if (vertexEquClass.empty() || vertexEquClass[i] == i)
			{
				createValueBuildersVisitor(vertexAt(i), i);
			}
			else
			{
				noRepresentative.push_back(i);
			}
		}
	}
	for (auto i : noRepresentative)
	{
		valueResultVector[i] = valueResultVector[vertexEquClass[i]];
	}
}

void CopySectionShallowPass::createSectionBuilders()
{
	sectionResultVector.resize(getSubgraphNum(), nullptr);
	sectionBuilderVector.resize(getSubgraphNum());
	for (uint32_t i = 0; i < getSubgraphNum(); i++)
	{
		auto section = subgraphAt(i);
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			auto& sectionBuilder = sectionBuilderVector[i];
			sectionBuilder = section->initBuilder();
			sectionBuilder.copySectionName(section);
			sectionResultVector[i] = sectionBuilder.getInstance();

			// SectionGenerator
			size_t call_num = sectionBuilder.getInstance()->getSectionCallVectorSize();
			for (size_t i = 0; i < call_num; i++)
			{
				auto call = section->getSectionCall(i);
				auto builder = initSectionCallBuilder(call);
				sectionBuilder.setSectionCall(i, builder);
				sectionCallMap.emplace(call, builder);
			}
		}
	}
}

void CopySectionShallowPass::createValueOperands()
{
	for (uint32_t i = 0; i < valueBuilderVector.size(); i++)
	{
		auto& builder = valueBuilderVector[i];
		if (auto variableValue = dynamic_cast<VariableValue*>(builder.getInstance()))
		{
			auto& sectionBuilder = sectionBuilderVector[getSubgraphByVertex(i)];
			createValueOperandsVisitor(vertexAt(i), builder, sectionBuilder);
		}
	}
}

void CopySectionShallowPass::createResult()
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

SectionCall::Builder CopySectionShallowPass::getSectionCallBuilder(SectionCall* src_call, Section::Builder sectionBuilder)
{
	auto iter = sectionCallMap.find(src_call);
	if (iter == sectionCallMap.end())
	{
		auto builder = initSectionCallBuilder(src_call);
		sectionBuilder.addSectionCall(builder);
		iter = sectionCallMap.emplace(src_call, builder).first;
	}
	return iter->second;
}

SectionCall::Builder CopySectionShallowPass::initSectionCallBuilder(SectionCall* src_call)
{
	SectionCall::Builder builder;
	auto src_callee = src_call->getCallee();
	auto dst_callee = mapSectionByParent(src_callee);
	assert(dst_callee != nullptr);
	builder.createInstance(dst_callee);
	for (auto src_param : src_callee->getParameterVector())
	{
		if (auto dst_param = dynamic_cast<InputValue*>(mapValueByParent(src_param)))
		{
			if (dst_param->getSection() == dst_callee)
			{
				auto src_arg = src_call->getArgument(src_param->getIndex());
				auto dst_arg = mapValue(src_arg);
				if (dst_param->isTemplate())
				{
					builder.setSpecializer(dst_param->getIndex(), dst_arg);
				}
				else
				{
					builder.setArgument(dst_param->getIndex(), dst_arg);
				}
			}
		}
	}
	if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(src_callee))
	{
		for (auto src_param : sectionGenerator->getTemplateVector())
		{
			if (auto dst_param = dynamic_cast<InputValue*>(mapValueByParent(src_param)))
			{
				if (dst_param->getSection() == dst_callee)
				{
					auto src_arg = src_call->getSpecializer(src_param->getIndex());
					auto dst_arg = mapValue(src_arg);
					if (dst_param->isTemplate())
					{
						builder.setSpecializer(dst_param->getIndex(), dst_arg);
					}
					else
					{
						builder.setArgument(dst_param->getIndex(), dst_arg);
					}
				}
			}
		}
	}
	return builder;
}

void CopySectionDeepPass::run()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	sectionResultVector.resize(getSubgraphNum(), nullptr);
	for (uint32_t i = 0; i < getSubgraphNum(); i++)
	{
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			runOnSubgraph(i);
		}
	}
	std::vector<Section*> dstSectionVector;
	for (auto section : getRootSections())
	{
		uint32_t i = subgraphId(section);
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			dstSectionVector.push_back(sectionResultVector[i]);
		}
	}
	result.load(dstSectionVector);
}

GraphValueProjection CopySectionDeepPass::run(GraphValueProjection src)
{
	setGraph(std::move(src));
	run();
	return std::move(result);
}

Value* CopySectionDeepPass::mapValue(Value* value) const
{
	return vertexExists(value) ? valueResultVector[vertexId(value)] : value;
}

Section* CopySectionDeepPass::mapSection(Section* section) const
{
	return subgraphExists(section) ? sectionResultVector[subgraphId(section)] : section;
}

void CopySectionDeepPass::runOnSubgraph(uint32_t i)
{
	CopySectionShallowPass subPass;
	subPass.parent = this;
	subPass.load({ subgraphAt(i) });
	subPass.run();
	updateResultVector(subPass);
}

void CopySectionDeepPass::updateResultVector(const CopySectionShallowPass& subPass)
{
	for (auto section : subPass.getSubgraphVector())
	{
		sectionResultVector[subgraphId(section)] = subPass.mapSection(section);
	}
	for (auto value : subPass.getVertexVector())
	{
		valueResultVector[vertexId(value)] = subPass.mapValue(value);
	}
}

void CopyLoopAndBodyPass::run()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	sectionResultVector.resize(getSubgraphNum(), nullptr);
	runOnBody();
	runOnLoop();
	result.load({ sectionResultVector[1] });
}

GraphValueProjection CopyLoopAndBodyPass::run(GraphLoopAndBody src)
{
	setGraph(std::move(src));
	run();
	return std::move(result);
}

Value* CopyLoopAndBodyPass::mapValue(Value* value) const
{
	return vertexExists(value) ? valueResultVector[vertexId(value)] : value;
}

Section* CopyLoopAndBodyPass::mapSection(Section* section) const
{
	return subgraphExists(section) ? sectionResultVector[subgraphId(section)] : section;
}

Value* CopyLoopAndBodyPass::mapValueByParent(Value* value) const
{
	return parent != nullptr ? parent->mapValue(value) : value;
}

Section* CopyLoopAndBodyPass::mapSectionByParent(Section* section) const
{
	return parent != nullptr ? parent->mapSection(section) : section;
}

void CopyLoopAndBodyPass::runOnBody()
{
	CopySectionShallowPass subPass;
	subPass.parent = this;
	subPass.load({ subgraphAt(0) });
	subPass.run();
	updateResultVector(subPass);
}

void CopyLoopAndBodyPass::runOnLoop()
{
	CopySectionShallowPass subPass;
	subPass.parent = this;
	subPass.load({ subgraphAt(1) });
	subPass.run();
	updateResultVector(subPass);
}

void CopyLoopAndBodyPass::updateResultVector(const CopySectionShallowPass& subPass)
{
	for (auto section : subPass.getSubgraphVector())
	{
		sectionResultVector[subgraphId(section)] = subPass.mapSection(section);
	}
	for (auto value : subPass.getVertexVector())
	{
		valueResultVector[vertexId(value)] = subPass.mapValue(value);
	}
}
