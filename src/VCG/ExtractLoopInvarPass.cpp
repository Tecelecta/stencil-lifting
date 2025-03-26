#define VCG_EXPORTS

#include "ExtractLoopInvarPass.h"

ExtractLoopInvarShallowPass::ExtractLoopInvarShallowPass()
{
	createValueBuildersVisitor.defaultFunction = [this](Value* value, uint32_t i)
		{
			valueLayerVector[i] = 2;
			valueResultEachLayerVector[i] = { value, value, value };
		};

	createValueBuildersVisitor.visitVariableValue = [this](VariableValue* value, uint32_t i)
		{
			if (getSubgraphByVertex(i) == 0)
			{
				if (isInvar(i))
				{
					valueLayerVector[i] = 2;
					InputValue::Builder builder0;
					valueResultEachLayerVector[i][0] = builder0.initInstance(value);
					valueBuilderEachLayerVector[i][0] = std::move(builder0);
					InputValue::Builder builder1;
					valueResultEachLayerVector[i][1] = builder1.initInstance(value);
					valueBuilderEachLayerVector[i][1] = std::move(builder1);
					Value::Builder builder2 = value->cloneBuilder();
					valueResultEachLayerVector[i][2] = builder2.getInstance();
					valueBuilderEachLayerVector[i][2] = std::move(builder2);
				}
				else
				{
					valueLayerVector[i] = 0;
					Value::Builder builder0 = value->cloneBuilder();
					valueResultEachLayerVector[i][0] = builder0.getInstance();
					valueBuilderEachLayerVector[i][0] = std::move(builder0);
				}
			}
			else // getSubgraphByVertex(i) == 1
			{
				if (isInvar(i))
				{
					valueLayerVector[i] = 2;
					InputValue::Builder builder1;
					valueResultEachLayerVector[i][1] = builder1.initInstance(value);
					valueBuilderEachLayerVector[i][1] = std::move(builder1);
					Value::Builder builder2 = value->cloneBuilder();
					valueResultEachLayerVector[i][2] = builder2.getInstance();
					valueBuilderEachLayerVector[i][2] = std::move(builder2);
				}
				else
				{
					valueLayerVector[i] = 1;
					Value::Builder builder1 = value->cloneBuilder();
					valueResultEachLayerVector[i][1] = builder1.getInstance();
					valueBuilderEachLayerVector[i][1] = std::move(builder1);
				}
			}
		};

	createValueBuildersVisitor.visitInputValue = [this](InputValue* value, uint32_t i)
		{
			if (value->isTemplate())
			{
				valueLayerVector[i] = 2;
				Value::Builder builder1 = value->cloneBuilder();
				valueResultEachLayerVector[i][1] = builder1.getInstance();
				valueBuilderEachLayerVector[i][1] = std::move(builder1);
				InputValue::Builder builder2;
				valueResultEachLayerVector[i][2] = builder2.initInstance(value);
				valueBuilderEachLayerVector[i][2] = std::move(builder2);
			}
			else
			{
				if (getSubgraphByVertex(i) == 0)
				{
					if (isInvar(i))
					{
						valueLayerVector[i] = 2;
						Value::Builder builder0 = value->cloneBuilder();
						valueResultEachLayerVector[i][0] = builder0.getInstance();
						valueBuilderEachLayerVector[i][0] = std::move(builder0);
						valueResultEachLayerVector[i][1] = valueResultEachLayerVector[getOperandsByVertex(i)[0]][1];
						valueResultEachLayerVector[i][2] = valueResultEachLayerVector[getOperandsByVertex(i)[0]][2];
					}
					else
					{
						valueLayerVector[i] = 0;
						Value::Builder builder0 = value->cloneBuilder();
						valueResultEachLayerVector[i][0] = builder0.getInstance();
						valueBuilderEachLayerVector[i][0] = std::move(builder0);
					}
				}
				else // getSubgraphByVertex(i) == 1
				{
					valueLayerVector[i] = 2;
					Value::Builder builder1 = value->cloneBuilder();
					valueResultEachLayerVector[i][1] = builder1.getInstance();
					valueBuilderEachLayerVector[i][1] = std::move(builder1);
					Value::Builder builder2 = value->cloneBuilder();
					valueResultEachLayerVector[i][2] = builder2.getInstance();
					valueBuilderEachLayerVector[i][2] = std::move(builder2);
				}
			}
		};


	createValueBuildersVisitor.visitBoundValue = [this](BoundValue* value, uint32_t i)
		{
			valueLayerVector[i] = 1;
			Value::Builder builder1 = value->cloneBuilder();
			valueResultEachLayerVector[i][1] = builder1.getInstance();
			valueBuilderEachLayerVector[i][1] = std::move(builder1);
		};

	createValueBuildersVisitor.visitResultValue = [this](ResultValue* value, uint32_t i)
		{
			auto src = mapValueByParent(value->getSrc());
			if (dynamic_cast<VariableValue*>(src) == nullptr)
			{
				valueLayerVector[i] = 2;
				valueResultEachLayerVector[i] = { src, src, src };
			}
			else
			{
				createValueBuildersVisitor.visitVariableValue(value, i);
			}
		};

	createValueBuildersVisitor.visitPhiValue = [this](PhiValue* value, uint32_t i)
		{
			valueLayerVector[i] = 1;
			Value::Builder builder1 = value->cloneBuilder();
			valueResultEachLayerVector[i][1] = builder1.getInstance();
			valueBuilderEachLayerVector[i][1] = std::move(builder1);
		};

	createValueOperandsVisitor.defaultFunction = [](Value* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer) {};

	createValueOperandsVisitor.visitInputValue = [this](InputValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer)
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
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer)
		{
			BoundValue::Builder builder;
			builder.setInstance(valueBuilder);
			SectionGenerator::Builder sectionGeneratorBuilder;
			sectionGeneratorBuilder.setInstance(sectionBuilder);
			sectionGeneratorBuilder.setBound(value->getIndex(), builder);
		};

	createValueOperandsVisitor.visitResultValue = [this](ResultValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer)
		{
			if (layer == 0)
			{
				auto retvar = dynamic_cast<VariableValue*>(mapValueByParent(value->getSrc()));
				assert(retvar != nullptr);
				ResultValue::Builder builder;
				builder.setInstance(valueBuilder);
				builder.setSrc(retvar);
				auto callBuilder = getSectionCallBuilder(value->getCall(), sectionBuilder);
				assert(retvar->getSection() == callBuilder.getInstance()->getCallee());
				callBuilder.addResultValue(builder);
			}
		};

	createValueOperandsVisitor.visitOperationValue = [this](OperationValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer)
		{
			assert(layer != 1);
			OperationValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getSrcVectorSize(); i++)
			{
				builder.setSrc(i, valueResultEachLayerVector[vertexId(value->getSrc(i))][layer]);
			}
			SimpleSection::Builder simpleSectionBuilder;
			simpleSectionBuilder.setInstance(sectionBuilder);
			simpleSectionBuilder.addOperationValue(builder);
		};

	createValueOperandsVisitor.visitPhiValue = [this](PhiValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder, uint32_t layer)
		{
			assert(layer == 1);
			PhiValue::Builder builder;
			builder.setInstance(valueBuilder);
			for (size_t i = 0; i < value->getIncomingVectorSize(); i++)
			{
				builder.setIncoming(i, valueResultEachLayerVector[vertexId(value->getIncoming(i))][layer]);
			}
			SectionGenerator::Builder sectionGeneratorBuilder;
			sectionGeneratorBuilder.setInstance(sectionBuilder);
			sectionGeneratorBuilder.addPhiValue(builder);
		};
}

void ExtractLoopInvarShallowPass::run()
{
	createValueBuilders();
	createSectionBuilders();
	createValueOperands();
	createBodyCall();
	createLoopCall();
	result.load({ sectionResultVector.back() });
}

Value* ExtractLoopInvarShallowPass::mapValue(Value* value) const
{
	return vertexExists(value) ? valueResultVector[vertexId(value)] : value;
}

Section* ExtractLoopInvarShallowPass::mapSection(Section* section) const
{
	return subgraphExists(section) ? sectionResultVector[subgraphId(section)] : section;
}

Value* ExtractLoopInvarShallowPass::mapValueByParent(Value* value) const
{
	return parent != nullptr ? parent->mapValue(value) : value;
}

Section* ExtractLoopInvarShallowPass::mapSectionByParent(Section* section) const
{
	return parent != nullptr ? parent->mapSection(section) : section;
}

void ExtractLoopInvarShallowPass::createValueBuilders()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	valueLayerVector.resize(getVertexNum(), ~0);
	valueBuilderEachLayerVector.resize(getVertexNum());
	valueResultEachLayerVector.resize(getVertexNum(), { nullptr, nullptr, nullptr });
	iterateSCC(*this);
}

void ExtractLoopInvarShallowPass::createSectionBuilders()
{
	bodyBuilder.createInstance(body->getContext());
	bodyBuilder.copySectionName(body);
	loopBuilder.setInstance(loop->initBuilder());
	loopBuilder.copySectionName(loop);
	outerBuilder.createInstance(body->getContext());
	sectionResultVector = { bodyBuilder.getInstance(), outerBuilder.getInstance() };
}

void ExtractLoopInvarShallowPass::createValueOperands()
{
	for (uint32_t i = 0; i < valueBuilderEachLayerVector.size(); i++)
	{
		createValueOperands(i, bodyBuilder, 0u);
		createValueOperands(i, loopBuilder, 1u);
		createValueOperands(i, outerBuilder, 2u);
	}
}

void ExtractLoopInvarShallowPass::createValueOperands(uint32_t i, Section::Builder sectionBuilder, uint32_t layer)
{
	auto& builder = valueBuilderEachLayerVector[i][layer];
	if (dynamic_cast<VariableValue*>(builder.getInstance()) != nullptr)
	{
		if (dynamic_cast<InputValue*>(builder.getInstance()) != nullptr && 
			(dynamic_cast<InputValue*>(vertexAt(i)) == nullptr || layer != 1))
		{
			InputValue::Builder paramBuilder;
			paramBuilder.setInstance(builder);
			sectionBuilder.addParameter(paramBuilder);
		}
		else
		{
			createValueOperandsVisitor(vertexAt(i), builder, sectionBuilder, layer);
		}
	}
}

void ExtractLoopInvarShallowPass::createBodyCall()
{
	callBodyBuilder.createInstance(bodyBuilder.getInstance());
	loopBuilder.setSectionCall(0, callBodyBuilder);
	sectionCallMap.emplace(loop->getSectionCall(0), callBodyBuilder);
	for (uint32_t i = 0; i < valueBuilderEachLayerVector.size(); i++)
	{
		if (auto param = dynamic_cast<InputValue*>(valueResultEachLayerVector[i][0]))
		{
			auto arg = valueResultEachLayerVector[i][1];
			if (arg == nullptr)
			{
				assert(getOperandsByVertex(i).size() == 1);
				arg = valueResultEachLayerVector[getOperandsByVertex(i)[0]][1];
			}
			assert(arg != nullptr);
			callBodyBuilder.setArgument(param->getIndex(), arg);
		}
	}
	for (auto resultValue : loop->getSectionCall(0)->getResultValueVector())
	{
		auto retvar = dynamic_cast<VariableValue*>(mapValue(resultValue->getSrc()));
		assert(retvar != nullptr);
		ResultValue::Builder builder;
		builder.setInstance(valueBuilderEachLayerVector[vertexId(resultValue)][1]);
		builder.setSrc(retvar);
		assert(retvar->getSection() == callBodyBuilder.getInstance()->getCallee());
		callBodyBuilder.addResultValue(builder);
	}
}

void ExtractLoopInvarShallowPass::createLoopCall()
{
	callLoopBuilder.createInstance(loopBuilder.getInstance());
	outerBuilder.addSectionCall(callLoopBuilder);
	for (uint32_t i = 0; i < valueBuilderEachLayerVector.size(); i++)
	{
		if (auto param = dynamic_cast<InputValue*>(valueResultEachLayerVector[i][1]))
		{
			auto arg = valueResultEachLayerVector[i][2];
			assert(arg != nullptr);
			if (param->isTemplate())
			{
				callLoopBuilder.setSpecializer(param->getIndex(), arg);
			}
			else
			{
				callLoopBuilder.setArgument(param->getIndex(), arg);
			}
		}
	}
	for (auto phiValue : loop->getPhiValueVector())
	{
		auto src = mapValue(phiValue);
		valueResultVector[vertexId(phiValue)] = callLoopBuilder.createResultValue(dynamic_cast<VariableValue*>(src), src->getName());
	}
}

SectionCall::Builder ExtractLoopInvarShallowPass::getSectionCallBuilder(SectionCall* src_call, Section::Builder sectionBuilder)
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

SectionCall::Builder ExtractLoopInvarShallowPass::initSectionCallBuilder(SectionCall* src_call)
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
				auto dst_arg = valueResultEachLayerVector[vertexId(src_arg)][2];
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
					auto dst_arg = valueResultEachLayerVector[vertexId(src_arg)][2];
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

bool ExtractLoopInvarShallowPass::runOnTrivial(uint32_t i)
{
	createValueBuildersVisitor(vertexAt(i), i);
	valueResultVector[i] = valueResultEachLayerVector[i][valueLayerVector[i]];
	return true;
}

bool ExtractLoopInvarShallowPass::runOnTypical(const vertex_list& scc)
{
	for (auto i : scc)
	{
		runOnTrivial(i);
	}
	return true;
}

bool ExtractLoopInvarShallowPass::isInvar(uint32_t i) const
{
	for (auto j : getOperandsByVertex(i))
	{
		if (valueLayerVector[j] != 2)
		{
			return false;
		}
	}
	return true;
}

void ExtractLoopInvarDeepPass::runOnSubgraph(uint32_t i)
{
	if (auto loop = dynamic_cast<IterateLoop*>(subgraphAt(i)))
	{
		runOnLoop(loop);
	}
	else if (auto loop = dynamic_cast<ParallelLoop*>(subgraphAt(i)))
	{
		runOnLoop(loop);
	}
	else
	{
		CopySectionDeepPass::runOnSubgraph(i);
	}
}

void ExtractLoopInvarDeepPass::runOnLoop(SectionGenerator* loop)
{
	ExtractLoopInvarShallowPass subPass;
	subPass.parent = this;
	subPass.loadLoop(loop);
	subPass.run();
	subPass.result.validate();
	for (auto section : subPass.getSubgraphVector())
	{
		sectionResultVector[subgraphId(section)] = subPass.mapSection(section);
	}
	for (auto value : subPass.getVertexVector())
	{
		valueResultVector[vertexId(value)] = subPass.mapValue(value);
	}
}
