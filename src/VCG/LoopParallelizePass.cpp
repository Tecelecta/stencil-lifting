#include "LoopParallelizePass.h"
#include "Context.h"

#ifdef _DEBUG
#include <sstream>
std::string viz_z3(z3::expr expr) {
	std::stringstream ss;
	ss << expr;
	return ss.str();
}
#endif

//#define STEP 4

LoopParallelizePass::LoopParallelizePass(z3::context& z3ctx) : z3ctx(z3ctx)
{
	createValueBuildersVisitor.defaultFunction = [this](Value* value, uint32_t i)
		{
			InputValue::Builder builder;
			builder.initInstance(value);
			valueBuilderVector[i] = builder;
			valueResultVector[i] = valueBuilderVector[i].getInstance();
			const auto& summary = summaryVector[i];
			switch (summary.form)
			{
			case Summary::Form::ELEM:
			case Summary::Form::INDEX:
				outerValueMap.emplace(summary.branches[0].elem, builder.getInstance());
				break;
			case Summary::Form::RANGE:
			case Summary::Form::ARRAY:
				outerValueMap.emplace(summary.base, builder.getInstance());
				break;
			default:
				assert(false);
				break;
			}
		};

	createValueBuildersVisitor.visitVariableValue = nullptr;

	createValueBuildersVisitor.visitBoundValue = [this](BoundValue* value, uint32_t i)
		{
			valueResultVector[i] = value;
		};

	createValueBuildersVisitor.visitPhiValue = [this](PhiValue* value, uint32_t i)
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				summaryBuildersMap.emplace(value, SummaryBuilders());
				ResultValue::Builder resultBuilder;
				resultBuilder.createInstance(context);
				valueBuilderVector[i] = resultBuilder;
				valueResultVector[i] = valueBuilderVector[i].getInstance();
			}
			else
			{
				valueResultVector[i] = value;
			}
		};

	createValueOperandsVisitor.visitInputValue = [this](InputValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			InputValue::Builder builder;
			builder.setInstance(valueBuilder);
			sectionBuilder.addParameter(builder);
		};

	createValueOperandsVisitor.visitBoundValue = [this](BoundValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			InputValue::Builder builder;
			builder.setInstance(valueBuilder);
			sectionBuilder.addParameter(builder);
		};

	createValueBuildersVisitor.visitResultValue = [this](ResultValue* value, uint32_t i)
		{
			valueResultVector[i] = value;
		};

	createValueOperandsVisitor.visitPhiValue = [this](PhiValue* value,
		Value::Builder valueBuilder, Section::Builder sectionBuilder)
		{
			auto& summaryBuilders = summaryBuildersMap.at(value);
			auto phi_value_loop = translatePhi(value, summaryVector[vertexId(value)], summaryBuilders);
			ResultValue::Builder builder;
			builder.setInstance(valueBuilder);
			builder.setSrcAndType(phi_value_loop);
			builder.copyName(phi_value_loop);
			summaryBuilders.loop.callBuilder.addResultValue(builder);
			return builder.getInstance();
		};
}

void LoopParallelizePass::createValueBuilders()
{
	valueResultVector.resize(getVertexNum(), nullptr);
	valueBuilderVector.resize(getVertexNum());
	for (uint32_t i = 0; i < getVertexNum(); i++)
	{
		createValueBuildersVisitor(vertexAt(i), i);
	}
}

void LoopParallelizePass::createSectionBuilders()
{
	SimpleSection::Builder builder;
	builder.createInstance(context);
	builder.copySectionName(getRootSection(0));
	sectionResultVector = { builder.getInstance()};
	sectionBuilderVector = { builder };
}

PhiValue* LoopParallelizePass::translatePhi(PhiValue* value, const Summary& summary, SummaryBuilders& summaryBuilders)
{
	// 
	auto numDims = value->getType().getNumDims();
	std::vector<bool> isLoop(numDims, false);
	std::vector<std::optional<uint32_t>> loopDim(numDims, std::nullopt);
	auto w = getArrayBound(z3ctx, numDims);
	uint32_t numLoops = 0;
	for (const auto& branch : summary.branches)
	{
		for (uint32_t i = 0; i < branch.index.size(); i++)
		{
//			if (proveEquals(w[i], branch.index[i].func)
//				|| proveEquals((w[i]-STEP)/STEP*STEP+STEP, branch.index[i].func))
			if (has(branch.index[i].func, "w_"))
            {
				if (!isLoop.at(i))
				{
					isLoop[i] = true;
					loopDim[i] = numLoops++;
				}
			}
		}
	}

	// 
	auto loop = summaryBuilders.loop.sectionBuilder.createInstance(context, numLoops);
	createParameters(outerValueMap, summaryBuilders.loop.sectionBuilder,
		summaryBuilders.loop.callBuilder, summaryBuilders.loop.valueMap);
	for (uint32_t j = 0; j < w.size(); j++)
	{
		if (loopDim[j].has_value())
		{
			auto index = loopDim[j].value();
			summaryBuilders.loop.valueMap.emplace(w[j], loop->getCounter(index));
		}
	}
	sectionBuilderVector[0].addSectionCall(summaryBuilders.loop.callBuilder);

	// 
	auto body = summaryBuilders.body.sectionBuilder.createInstance(context);
	createParameters(summaryBuilders.loop.valueMap, summaryBuilders.body.sectionBuilder,
		summaryBuilders.body.callBuilder, summaryBuilders.body.valueMap);
	summaryBuilders.loop.sectionBuilder.setBodyCall(summaryBuilders.body.callBuilder);
	auto array_value_body = dynamic_cast<VariableValue*>(summaryBuilders.body.valueMap.at(summary.base));

	std::unordered_map<Z3_ast, Value*> savedValueMap = summaryBuilders.body.valueMap;
	summaryBuilders.branches.resize(summary.branches.size());
	for (size_t i = 0; i < summary.branches.size(); i++)
	{
		// 
		auto& branchBuilders = summaryBuilders.branches[i];
		branchBuilders.branch.sectionBuilder.createInstance(context);
		for (uint32_t j = 0; j < w.size(); j++)
		{
			if (!loopDim[j].has_value())
			{
				summaryBuilders.body.valueMap.emplace(w[j], translateExprAndSaveResult(summary.branches[i].index[j].func,
					summaryBuilders.body.sectionBuilder, summaryBuilders.body.valueMap));
			}
		}
		createParameters(summaryBuilders.body.valueMap, branchBuilders.branch.sectionBuilder,
			branchBuilders.branch.callBuilder, branchBuilders.branch.valueMap);
		summaryBuilders.body.sectionBuilder.addSectionCall(branchBuilders.branch.callBuilder);

		// 
		auto branch = branchBuilders.branch.sectionBuilder.getInstance();
		auto cond_value = translateExprAndSaveResult(summary.branches[i].cond,
			summaryBuilders.body.sectionBuilder, summaryBuilders.body.valueMap);
		branchBuilders.branch.callBuilder.setSpecializer(0, cond_value);

		// 
		auto leaf = branchBuilders.leaf.sectionBuilder.createInstance(context);
		createParameters(branchBuilders.branch.valueMap, branchBuilders.leaf.sectionBuilder,
			branchBuilders.leaf.callBuilder, branchBuilders.leaf.valueMap);
		branchBuilders.branch.sectionBuilder.setIfBranch(branchBuilders.leaf.callBuilder);
		SectionCall::Builder callElseBuilder;
		callElseBuilder.createInstance(SimpleSection::Builder().createInstance(context));
		branchBuilders.branch.sectionBuilder.setElseBranch(callElseBuilder);
		auto base_value_leaf = branchBuilders.leaf.valueMap.at(summary.base);
		std::vector<Value*> index_values(base_value_leaf->getType().getNumDims());
		for (size_t j = 0; j < index_values.size(); j++)
		{
			index_values[j] = translateExprAndSaveResult(summary.branches[i].index[j].func,
				branchBuilders.leaf.sectionBuilder, branchBuilders.leaf.valueMap);
		}
		auto elem_value = translateExprAndSaveResult(summary.branches[i].elem,
			branchBuilders.leaf.sectionBuilder, branchBuilders.leaf.valueMap);
		auto store_value = branchBuilders.leaf.sectionBuilder.createArraySetValue(
			base_value_leaf, index_values, elem_value, base_value_leaf->getName());

		// 
		auto result_value_leaf = branchBuilders.leaf.callBuilder.createResultValue(store_value, store_value->getName());
		auto base_value_branch = branchBuilders.branch.valueMap.at(summary.base);
		auto phi_value_branch = branchBuilders.branch.sectionBuilder.createPhiValue(
			base_value_branch->getType(), result_value_leaf, base_value_branch, base_value_branch->getName());
		array_value_body = branchBuilders.branch.callBuilder.createResultValue(phi_value_branch, phi_value_branch->getName());
		summaryBuilders.body.valueMap = savedValueMap;
		summaryBuilders.body.valueMap[summary.base] = array_value_body;

#ifdef _DEBUG
		leaf->validate();
		branch->validate();
#endif // _DEBUG
	}

	// 
	auto result_value_body = summaryBuilders.body.callBuilder.createResultValue(array_value_body, array_value_body->getName());
	auto base_value_loop = summaryBuilders.loop.valueMap.at(summary.base);
	auto phi_value_loop = summaryBuilders.loop.sectionBuilder.createPhiValue(
		base_value_loop->getType(), base_value_loop, result_value_body, base_value_loop->getName());
#ifdef _DEBUG
	body->validate();
	loop->validate();
#endif // _DEBUG
	return phi_value_loop;
}

Value* LoopParallelizePass::translateExprAndSaveResult(z3::expr expr, SimpleSection::Builder builder,
	std::unordered_map<Z3_ast, Value*>& valueMap)
{
#ifdef _DEBUG
	auto s = viz_z3(expr);
#endif 
	auto iter = valueMap.find(expr);
	if (iter == valueMap.end())
	{
#ifdef _DEBUG
		std::cout << "creating value for expr:" << s << std::endl;
#endif
		auto result = translateExpr(expr, builder, valueMap);
		iter = valueMap.emplace(expr, result).first;
	}
#ifdef _DEBUG
	else {
		std::cout << "found value for expr:" << expr << std::endl;
	}
#endif
	return iter->second;
}

Value* LoopParallelizePass::translateExpr(z3::expr expr, SimpleSection::Builder builder,
	std::unordered_map<Z3_ast, Value*>& valueMap)
{
	auto z3_sort = expr.get_sort();
	Operation::Data opData;
	auto decl = expr.decl();
	auto kind = decl.decl_kind();
	switch (kind)
	{
	case Z3_OP_TRUE:
		return context->getTrueValue();
	case Z3_OP_FALSE:
		return context->getFalseValue();
	case Z3_OP_EQ:
		return translateRelationExpr(expr, builder, valueMap, "eq");
	case Z3_OP_DISTINCT:
		return translateRelationExpr(expr, builder, valueMap, "ne");
	case Z3_OP_ITE:
		return builder.createSelectValue(getTypeFromZ3(context, z3_sort), translateExprAndSaveResult(expr.arg(0), builder, valueMap),
			translateExprAndSaveResult(expr.arg(1), builder, valueMap), translateExprAndSaveResult(expr.arg(2), builder, valueMap));
	case Z3_OP_AND:
		if (z3_sort.is_bool())
		{
			opData.name = context->getString("Logic.and");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getLogicType());
			opData.returnType = context->getLogicType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_OR:
		if (z3_sort.is_bool())
		{
			opData.name = context->getString("Logic.or");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getLogicType());
			opData.returnType = context->getLogicType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_XOR:
		if (z3_sort.is_bool())
		{
			opData.name = context->getString("Logic.xor");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getLogicType());
			opData.returnType = context->getLogicType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_NOT:
		if (z3_sort.is_bool())
		{
			opData.name = context->getString("Logic.not");
			opData.parameterTypes = { context->getLogicType() };
			opData.returnType = context->getLogicType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_IMPLIES:
		break;
	case Z3_OP_ANUM:
		if (z3_sort.is_int())
		{
			return context->createConstantValue(context->getIntegerType(), context->getInteger(big_int(expr.get_decimal_string(0))));
		}
		if (z3_sort.is_real())
		{
			auto x = expr.get_decimal_string(20);
			if (x.back() == '?')
			{
				x.pop_back();
			}
			return context->createConstantValue(context->getRealType(), context->getDecimal(big_decimal(x)));
		}
		break;
	case Z3_OP_LE:
		return translateRelationExpr(expr, builder, valueMap, "le");
	case Z3_OP_GE:
		return translateRelationExpr(expr, builder, valueMap, "ge");
	case Z3_OP_LT:
		return translateRelationExpr(expr, builder, valueMap, "lt");
	case Z3_OP_GT:
		return translateRelationExpr(expr, builder, valueMap, "gt");
	case Z3_OP_ADD:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.add");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getIntegerType());
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		if (z3_sort.is_real())
		{
			opData.name = context->getString("Real.add");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getRealType());
			opData.returnType = context->getRealType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_SUB:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.sub");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getIntegerType());
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		if (z3_sort.is_real())
		{
			opData.name = context->getString("Real.sub");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getRealType());
			opData.returnType = context->getRealType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_UMINUS:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.neg");
			opData.parameterTypes = { expr.num_args(), context->getIntegerType() };
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		if (z3_sort.is_real())
		{
			opData.name = context->getString("Real.neg");
			opData.parameterTypes = { expr.num_args(), context->getRealType() };
			opData.returnType = context->getRealType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_MUL:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.mul");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getIntegerType());
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		if (z3_sort.is_real())
		{
			opData.name = context->getString("Real.mul");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getRealType());
			opData.returnType = context->getRealType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_DIV:
		if (z3_sort.is_real())
		{
			opData.name = context->getString("Real.div");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getRealType());
			opData.returnType = context->getRealType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_IDIV:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.div");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getIntegerType());
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_REM:
		break;
	case Z3_OP_MOD:
		if (z3_sort.is_int())
		{
			opData.name = context->getString("Integer.mod");
			opData.parameterTypes = std::vector<Type>(expr.num_args(), context->getIntegerType());
			opData.returnType = context->getIntegerType();
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	case Z3_OP_TO_REAL:
		if (z3_sort.is_int() || z3_sort.is_real())
		{
			return translateExprAndSaveResult(expr.arg(0), builder, valueMap);
		}
		break;
	case Z3_OP_TO_INT:
		break;
	case Z3_OP_POWER:
		break;
	case Z3_OP_SELECT: {
		auto srcVector = translateSubExpr(expr, builder, valueMap);
		auto indexVector = decltype(srcVector)(srcVector.begin() + 1, srcVector.end());
		return builder.createArrayGetValue(srcVector.front(), indexVector);
	}
	case Z3_OP_UNINTERPRETED:
		if (expr.num_args() == 0)
		{
#ifdef _DEBUG
			auto iter = valueMap.find(expr);
			if (iter == valueMap.end())
			{
				auto tupleGet = translateTupleGet(expr, builder, valueMap);
				if (tupleGet == nullptr)
				{
					// tuplefree variable
					if (expr.to_string()[0] == 'w') {
						throw std::logic_error("bound value");
					} else {
						std::cerr << expr << std::endl;
						throw std::logic_error("Operand not found");
					}
				}
				return tupleGet;
			}
			return iter->second;
#else
			return valueMap.at(expr);
#endif // _DEBUG
		}
		else
		{
			opData.name = context->getString(decl.name().str());
			for (uint32_t j = 0; j < expr.num_args(); j++)
			{
				opData.parameterTypes.push_back(getTypeFromZ3(context, decl.domain(j)));
			}
			opData.returnType = getTypeFromZ3(context, decl.range());
			return builder.createOperationValue(context->getOperation(opData), translateSubExpr(expr, builder, valueMap));
		}
		break;
	default:
		break;
	}
#ifdef _DEBUG
	std::cerr << expr << std::endl;
#endif // _DEBUG
	throw std::logic_error("Unsupported operator");
}

Value* LoopParallelizePass::translateRelationExpr(z3::expr expr, SimpleSection::Builder builder,
	std::unordered_map<Z3_ast, Value*>& valueMap, const char* opName)
{
	Operation::Data opData;
	auto srcVector = translateSubExpr(expr, builder, valueMap);
	opData.name = context->getString(srcVector[0]->getType().getName().str() + '.' + opName);
	opData.parameterTypes = std::vector<Type>(expr.num_args(), srcVector[0]->getType());
	opData.returnType = context->getLogicType();
	return builder.createOperationValue(context->getOperation(opData), srcVector);
}

std::vector<Value*> LoopParallelizePass::translateSubExpr(z3::expr expr, SimpleSection::Builder builder,
	std::unordered_map<Z3_ast, Value*>& valueMap)
{
	std::vector<Value*> result;
	for (uint32_t i = 0; i < expr.num_args(); i++)
	{
		result.push_back(translateExprAndSaveResult(expr.arg(i), builder, valueMap));
	}
	return result;
}

void LoopParallelizePass::createParameters(const std::unordered_map<Z3_ast, Value*>& srcValueMap, Section::Builder dstSectionBuilder,
	SectionCall::Builder& dstCallBuilder, std::unordered_map<Z3_ast, Value*>& dstValueMap)
{
	std::vector<std::pair<Z3_ast, InputValue::Builder>> buffer;
	for (auto [ast, outer] : srcValueMap)
	{
		InputValue::Builder paramBuilder;
		auto inner = paramBuilder.initInstance(outer);
		if (auto outer_bound = dynamic_cast<BoundValue*>(outer))
		{
			paramBuilder.setIndex(outer_bound->getIndex());
		}
		else if (auto outer_input = dynamic_cast<InputValue*>(outer))
		{
			paramBuilder.setIndex(outer_input->getIndex() + (outer_input->isTemplate() ? 200 : 100));
		}
		else
		{
			paramBuilder.setIndex(10000);
		}
		buffer.emplace_back(ast, paramBuilder);
	}
	std::sort(buffer.begin(), buffer.end(),
		[](const std::pair<Z3_ast, InputValue::Builder>& a, const std::pair<Z3_ast, InputValue::Builder>& b)
		{
			return a.second.getInstance()->getIndex() < b.second.getInstance()->getIndex();
		});
	for (auto [ast, paramBuilder] : buffer)
	{
		dstSectionBuilder.addParameter(paramBuilder);
		dstValueMap.emplace(ast, paramBuilder.getInstance());
	}
	dstCallBuilder.createInstance(dstSectionBuilder.getInstance());
	for (auto [ast, paramBuilder] : buffer)
	{
		dstCallBuilder.setArgument(paramBuilder.getInstance()->getIndex(), srcValueMap.at(ast));
	}
}

OperationValue* LoopParallelizePass::translateTupleGet(z3::expr expr, SimpleSection::Builder builder,
	std::unordered_map<Z3_ast, Value*>& valueMap)
{
	auto iter = tupleElemMap.find(expr);
	if (iter == tupleElemMap.end())
	{
		return nullptr;
	}
	auto base = valueMap.at(iter->second.first);
	auto index = iter->second.second;
	return builder.createTupleGetValue(base, index);
}
