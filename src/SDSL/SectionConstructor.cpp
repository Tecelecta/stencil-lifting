#define SDSL_EXPORTS

#include "SectionConstructor.h"
#include "VCG/ConstantFoldingPass.h"

#define CONSTANT_NUMERAL DEC_INTEGER: case Token::Category::BIN_INTEGER: case Token::Category::HEX_INTEGER: case Token::Category::DEC_RATIONAL
#define RIGHT_VALUE_EXPRESSION PRIMARY_EXPRESSION: case AST::Category::UNARY_EXPRESSION: case AST::Category::BINARY_EXPRESSION: case AST::Category::FUNCTION_CALL: case AST::Category::ARRAY_ELEMENT: case AST::Category::STRUCT_ELEMENT: case AST::Category::TUPLE_INITIALIZER

static bool isImmediateConstant(Value* value)
{
	if (auto constant = dynamic_cast<ConstantValue*>(value))
	{
		return !constant->getName().isValid();
	}
	return false;
}

SectionConstructor::SectionConstructor(ErrorHandler& errorHandler, Context* context)
	: OperationMatcher(errorHandler), context(context)
{
	assert(context != nullptr);

	symbolTable.defineSymbol(context->getString("Logic"), context->getLogicType());
	symbolTable.defineSymbol(context->getString("Integer"), context->getIntegerType());
	symbolTable.defineSymbol(context->getString("Rational"), context->getRationalType());
	symbolTable.defineSymbol(context->getString("Real"), context->getRealType());
	symbolTable.defineSymbol(context->getString("Complex"), context->getComplexType());

	symbolTable.defineSymbol(context->getString("bit"), context->getBitType());
	symbolTable.defineSymbol(context->getString("byte"), context->getByteType());
	symbolTable.defineSymbol(context->getString("int8"), context->getInt8Type());
	symbolTable.defineSymbol(context->getString("int16"), context->getInt16Type());
	symbolTable.defineSymbol(context->getString("int32"), context->getInt32Type());
	symbolTable.defineSymbol(context->getString("int64"), context->getInt64Type());
	symbolTable.defineSymbol(context->getString("float16"), context->getFloat16Type());
	symbolTable.defineSymbol(context->getString("float32"), context->getFloat32Type());
	symbolTable.defineSymbol(context->getString("float64"), context->getFloat64Type());

	symbolTable.defineSymbol(context->getString("true"), ValueDefinition{ ValueDefinition::Position::VARIABLE, context->getTrueValue() });
	symbolTable.defineSymbol(context->getString("false"), ValueDefinition{ ValueDefinition::Position::VARIABLE, context->getFalseValue() });
}

bool SectionConstructor::runOnCompileUnit(AST* ast, std::vector<FunctionDefinition>& allFunctions)
{
	assert(ast->category == AST::Category::COMPILE_UNIT);
	defaultLoopStep = context->createConstantValue(context->getIntegerType(),
		context->getInteger(1), context->getString("step"));

	bool success = true;
	std::unordered_set<Section*> allSections;
	for (const auto& child : ast->children)
	{
		switch (child->category)
		{
		case AST::Category::FUNCTION_DEFINITION: {
			FunctionDefinition fn;
			if (!runOnFunctionDefinition(child.get(), fn))
			{
				success = false;
			}
			else
			{
				allFunctions.emplace_back(std::move(fn));
			}
			break;
		}
		case AST::Category::CONST_DEFINITION:
			success = runOnConstantDefinition(child.get()) && success;
			break;
		case AST::Category::STRUCT_DEFINITION:
			success = runOnStructDefinition(child.get()) && success;
			break;
		default:
			break;
		}
	}
	return success;
}

void SectionConstructor::assertTokenNode(AST* ast, Token::Category category)
{
	assert(ast->category == AST::Category::TERMINAL);
	assert(ast->firstToken.category == category);
}

void SectionConstructor::assertTokenNode(AST* ast, Token::Category category, std::string_view text)
{
	assertTokenNode(ast, category);
	assert(ast->firstToken.text == text);
}

bool SectionConstructor::runOnFunctionDefinition(AST* ast, FunctionDefinition& fn)
{
	assert(ast->category == AST::Category::FUNCTION_DEFINITION);
	assert(ast->children.size() == 8);
	assertTokenNode(ast->children[0].get(), Token::Category::FUNC);
	assertTokenNode(ast->children[2].get(), Token::Category::ASG);
	assertTokenNode(ast->children[6].get(), Token::Category::END);
	assertTokenNode(ast->children[7].get(), Token::Category::FUNC);

	SimpleSection::Builder sectionBuilder;
	fn.section = sectionBuilder.createInstance(context);
	assert(ast->children[3]->category == AST::Category::FUNCTION_NAME);
	assert(ast->children[3]->children.size() == 1);
	sectionBuilder.setSectionName(ast->children[3]->firstToken.text);
	bool success = sectionBuilder.getInstance()->getName().isValid();
	if (success)
	{
		symbolTable.defineSymbol(fn.section->getName(), fn);
		symbolTable.pushLayer();
		std::vector<String> returnSymbols;
		success = success && runOnFunctionReturn(ast->children[1].get(), sectionBuilder, returnSymbols);
		success = success && runOnFunctionParameter(ast->children[4].get(), sectionBuilder);
		success = success && runOnSimpleRegion(ast->children[5].get(), sectionBuilder);
		fn = std::get<FunctionDefinition>(symbolTable.findSymbol(fn.section->getName()));
		if (success)
		{
			std::vector<Value*> returnValues;
			for (auto symbol : returnSymbols)
			{
				auto valueDefinition = std::get<ValueDefinition>(symbolTable.findSymbol(symbol));
				fn.returnValues.push_back(valueDefinition.value);
			}
		}
		symbolTable.popLayer();
		symbolTable.updateSymbol(fn.section->getName(), fn);
	}
	return success;
}

bool SectionConstructor::runOnFunctionParameter(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->category == AST::Category::FUNCTION_PARAMETER);
	bool success = true;
	InputValue::Builder parameterBuilder;
	InputValue* inputValue = nullptr;
	for (size_t i = 0; i < ast->children.size(); i++)
	{
		switch (ast->children[i]->category)
		{
		case AST::Category::TERMINAL:
			switch (ast->children[i]->firstToken.category)
			{
			case Token::Category::LPAREN:
				assert(i == 0);
				break;
			case Token::Category::RPAREN:
				assert(i == ast->children.size() - 1);
				break;
			case Token::Category::COMMA:
				assert(i != 0 && i % 3 == 0);
				break;
			case Token::Category::IDENTIFIER: {
				auto symbol = context->getString(ast->children[i]->firstToken.text);
				switch (i % 3)
				{
				case 1: // 
					inputValue = parameterBuilder.createInstance(context);
					parameterBuilder.setType(getTypeObject(ast->children[i].get()));
					success = success && inputValue->getType() != nullptr;
					break;
				case 2: // 
					parameterBuilder.setName(symbol);
					sectionBuilder.addParameter(parameterBuilder);
					symbolTable.defineSymbol(symbol, ValueDefinition{ ValueDefinition::Position::PARAMETER, inputValue });
					break;
				default:
					assert(false);
				}
				break;
			}
			default:
				assert(false);
			}
			break;
		case AST::Category::ARRAY_ELEMENT:
			assert(i % 3 == 1);
			inputValue = parameterBuilder.createInstance(context);
			parameterBuilder.setType(getTypeObject(ast->children[i].get()));
			success = success && inputValue->getType().cast<ArrayType>() != nullptr;
			break;
		default:
			assert(false);
		}
	}
	return success;
}

bool SectionConstructor::runOnFunctionReturn(AST* ast, SimpleSection::Builder& sectionBuilder, std::vector<String>& returnSymbols)
{
	assert(ast->category == AST::Category::FUNCTION_RETURN_VALUE);
	bool success = true;
	InvalidValue::Builder returnBuilder;
	InvalidValue* returnValue = nullptr;
	for (size_t i = 0; i < ast->children.size(); i++)
	{
		switch (ast->children[i]->category)
		{
		case AST::Category::TERMINAL:
			switch (ast->children[i]->firstToken.category)
			{
			case Token::Category::COMMA:
				assert(i % 3 == 2);
				break;
			case Token::Category::IDENTIFIER: {
				auto symbol = context->getString(ast->children[i]->firstToken.text);
				switch (i % 3)
				{
				case 0: // 
					returnBuilder.createInstance(context);
					returnBuilder.setType(getTypeObject(ast->children[i].get()));
					returnValue = returnBuilder.getInstance();
					success = success && returnValue->getType() != nullptr;
					break;
				case 1: // 
					returnBuilder.setName(symbol);
					symbolTable.defineSymbol(symbol, ValueDefinition{ ValueDefinition::Position::RETURN, returnValue });
					returnSymbols.push_back(symbol);
					break;
				default:
					assert(false);
				}
				break;
			}
			default:
				assert(false);
			}
			break;
		case AST::Category::ARRAY_ELEMENT:
			assert(i % 3 == 0);
			returnBuilder.createInstance(context);
			returnBuilder.setType(getTypeObject(ast->children[i].get()));
			returnValue = returnBuilder.getInstance();
			success = success && returnValue->getType().cast<ArrayType>() != nullptr;
			break;
		default:
			assert(false);
		}
	}
	return success;
}

bool SectionConstructor::runOnSimpleRegion(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	bool success = true;
	for (const auto& child : ast->children)
	{
		switch (child->category)
		{
		case AST::Category::FUNCTION_DEFINITION: {
			FunctionDefinition fn;
			if (!runOnFunctionDefinition(child.get(), fn))
			{
				success = false;
			}
			break;
		}
		case AST::Category::VARIABLE_DEFINITION:
			success = runOnVariableDefinition(child.get(), sectionBuilder) && success;
			break;
		case AST::Category::CONST_DEFINITION:
			success = runOnConstantDefinition(child.get()) && success;
			break;
		case AST::Category::STRUCT_DEFINITION:
			success = runOnStructDefinition(child.get()) && success;
			break;
		case AST::Category::IF_STATEMENT:
			success = runOnIfStatement(child.get(), sectionBuilder) && success;
			break;
		case AST::Category::DO_STATEMENT:
			success = runOnDoStatement(child.get(), sectionBuilder) && success;
			break;
		case AST::Category::FOR_STATEMENT:
			success = runOnForStatement(child.get(), sectionBuilder) && success;
			break;
		case AST::Category::ASSIGN_STATEMENT:
			success = runOnAssignStatement(child.get(), sectionBuilder) && success;
			break;
		case AST::Category::TERMINAL:
			break;
		default:
			assert(false);
		}
	}
	return success;
}

bool SectionConstructor::runOnSimpleRegionPushLayer(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	symbolTable.pushLayer();
	bool success = runOnSimpleRegion(ast, sectionBuilder);
	symbolTable.popLayer();
	return success;
}

static VariableValue* dynamic_cast_operation_or_result_value(Value* value)
{
	if (auto innerValue = dynamic_cast<OperationValue*>(value))
	{
		return innerValue;
	}
	if (auto resultValue = dynamic_cast<ResultValue*>(value))
	{
		return resultValue;
	}
	return nullptr;
}

bool SectionConstructor::runOnIfStatement(AST* ast, SimpleSection::Builder& outerBuilder)
{
	assert(ast->category == AST::Category::IF_STATEMENT);
	assertTokenNode(ast->children[0].get(), Token::Category::IF);

	// 
	bool success = true;
	auto conditon = getRightValueOperand(ast->children[1].get(), outerBuilder);
	if (conditon->getType() != context->getLogicType())
	{
		errorHandler.error("Branching condition must be Logic type", ast->children[1]->firstToken.line, ast->children[1]->firstToken.column);
		success = false;
	}

	// 
	BinaryBranch::Builder branchBuilder;
	auto branch = branchBuilder.createInstance(context);
	auto initial_symbol_meaning_outer = symbolTable.getTopVariable();
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_outer)
	{
		auto param = branchBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}

	// if
	assert(ast->children[2]->category == AST::Category::IF_BODY);
	auto initial_symbol_meaning_branch = symbolTable.getTopVariable();
	SimpleSection::Builder ifBuilder;
	auto ifBody = ifBuilder.createInstance(context);
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_branch)
	{
		auto param = ifBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}
	SectionCall::Builder callIfBuilder;
	callIfBuilder.createInstance(ifBody);
	for (auto param : ifBody->getParameterVector())
	{
		auto arg = initial_symbol_meaning_branch.at(param->getName());
		callIfBuilder.setArgument(param->getIndex(), arg);
	}
	branchBuilder.setIfBranch(callIfBuilder);
	runOnSimpleRegionPushLayer(ast->children[2].get(), ifBuilder);
	std::unordered_map<String, VariableValue*> final_symbol_meaning_if;
	for (auto& [symbol, initValue] : initial_symbol_meaning_branch)
	{
		auto finalValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto innerValue = dynamic_cast_operation_or_result_value(finalValue))
		{
			auto resultValue = callIfBuilder.createResultValue(innerValue, symbol);
			final_symbol_meaning_if.emplace(symbol, resultValue);
		}
		else
		{
			final_symbol_meaning_if.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();

	std::unordered_map<String, VariableValue*> final_symbol_meaning_else;
	assert(ast->children[3]->category == AST::Category::TERMINAL);
	SimpleSection::Builder elseBuilder;
	auto elseBody = elseBuilder.createInstance(context);
	if (ast->children[3]->firstToken.category == Token::Category::ELSE)
	{
		// else
		symbolTable.pushLayer();
		for (const auto& [symbol, arg] : initial_symbol_meaning_branch)
		{
			auto param = elseBuilder.createParameter(arg->getType(), symbol);
			symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
		}
		SectionCall::Builder callElseBuilder;
		callElseBuilder.createInstance(elseBody);
		for (auto param : elseBody->getParameterVector())
		{
			auto arg = initial_symbol_meaning_branch.at(param->getName());
			callElseBuilder.setArgument(param->getIndex(), arg);
		}
		branchBuilder.setElseBranch(callElseBuilder);
		if (ast->children[4]->category == AST::Category::ELSE_BODY)
		{
			runOnSimpleRegionPushLayer(ast->children[4].get(), elseBuilder);
			assertTokenNode(ast->children[5].get(), Token::Category::END);
			assertTokenNode(ast->children[6].get(), Token::Category::IF);
			assert(ast->children.size() == 7);
		}
		else
		{
			// else if
			assert(ast->children[4]->category == AST::Category::IF_STATEMENT);
			runOnIfStatement(ast->children[4].get(), elseBuilder);
			assert(ast->children.size() == 5);
		}
		for (auto& [symbol, initValue] : initial_symbol_meaning_branch)
		{
			auto finalValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
			if (auto innerValue = dynamic_cast_operation_or_result_value(finalValue))
			{
				auto resultValue = callElseBuilder.createResultValue(innerValue, symbol);
				final_symbol_meaning_else.emplace(symbol, resultValue);
			}
			else
			{
				final_symbol_meaning_else.emplace(symbol, initValue);
			}
		}
		symbolTable.popLayer();
	}
	else
	{
		// else
		assertTokenNode(ast->children[3].get(), Token::Category::END);
		assertTokenNode(ast->children[4].get(), Token::Category::IF);
		assert(ast->children.size() == 5);
		SectionCall::Builder callElseBuilder;
		callElseBuilder.createInstance(elseBody);
		branchBuilder.setElseBranch(callElseBuilder);
		final_symbol_meaning_else = initial_symbol_meaning_branch;
	}

	// 
	std::unordered_map<String, VariableValue*> final_symbol_meaning_branch;
	for (auto& [symbol, initValue] : initial_symbol_meaning_branch)
	{
		auto ifValue = final_symbol_meaning_if.at(symbol);
		auto elseValue = final_symbol_meaning_else.at(symbol);
		if (ifValue == elseValue)
		{
			final_symbol_meaning_branch.emplace(symbol, ifValue);
		}
		else
		{
			auto phiValue = branchBuilder.createPhiValue(initValue->getType(), ifValue, elseValue, symbol);
			final_symbol_meaning_branch.emplace(symbol, phiValue);
		}
	}
	for (const auto& [symbol, value] : final_symbol_meaning_branch)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}

	// 
	BinaryBranch::CallBuilder callBranchBuilder;
	auto call_branch = callBranchBuilder.createInstance(branch);
	callBranchBuilder.setCondition(conditon);
	outerBuilder.addSectionCall(callBranchBuilder);
	std::unordered_map<String, VariableValue*> final_symbol_meaning_outer;
	for (auto& [symbol, initValue] : initial_symbol_meaning_outer)
	{
		auto finalValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto phiValue = dynamic_cast<PhiValue*>(finalValue))
		{
			auto resultValue = callBranchBuilder.createResultValue(phiValue, symbol);
			final_symbol_meaning_outer.emplace(symbol, resultValue);
		}
		else
		{
			final_symbol_meaning_outer.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();
	for (auto param : branch->getParameterVector())
	{
		auto arg = initial_symbol_meaning_outer.at(param->getName());
		callBranchBuilder.setArgument(param->getIndex(), arg);
	}
	for (const auto& [symbol, value] : final_symbol_meaning_outer)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}
	return success;
}

bool SectionConstructor::runOnDoStatement(AST* ast, SimpleSection::Builder& outerBuilder)
{
	assert(ast->category == AST::Category::DO_STATEMENT);
	assert(ast->children.size() == 8 || ast->children.size() == 9);
	assertTokenNode(ast->children[0].get(), Token::Category::DO);

	// 
	assert(ast->children[1]->category == AST::Category::LOOP_VARIABLE);
	assertTokenNode(ast->children[1]->children[0].get(), Token::Category::IDENTIFIER);
	auto counterName = context->getString(ast->children[1]->firstToken.text);
	assertTokenNode(ast->children[2].get(), Token::Category::ASG);
	assert(ast->children[3]->category == AST::Category::LOOP_HEAD);
	auto head = getRightValueOperand(ast->children[3]->children[0].get(), outerBuilder);
	assert(ast->children[4]->category == AST::Category::LOOP_TAIL);
	auto tail = getRightValueOperand(ast->children[4]->children[0].get(), outerBuilder);
	Value* step = nullptr;
	if (ast->children.size() == 9)
	{
		assert(ast->children[5]->category == AST::Category::LOOP_STEP);
		step = getRightValueOperand(ast->children[5]->children[0].get(), outerBuilder);
	}
	else
	{
		step = defaultLoopStep;
	}
	bool success = head != nullptr && tail != nullptr && step != nullptr;

	// 
	auto initial_symbol_meaning_outer = symbolTable.getTopVariable();
	IterateLoop::Builder loopBuilder;
	auto loop = loopBuilder.createInstance(context);
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_outer)
	{
		auto param = loopBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}
	symbolTable.defineSymbol(counterName, ValueDefinition{ ValueDefinition::Position::VARIABLE, loop->getCounter()});

	// 
	auto initial_symbol_meaning_loop = symbolTable.getTopVariable();
	SimpleSection::Builder bodyBuilder;
	auto body = bodyBuilder.createInstance(context);
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_loop)
	{
		auto param = bodyBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}

	// 
	assert(ast->children[ast->children.size() - 3]->category == AST::Category::LOOP_BODY);
	SectionCall::Builder callInnerBuilder;
	callInnerBuilder.createInstance(body);
	loopBuilder.setBodyCall(callInnerBuilder);
	runOnSimpleRegionPushLayer(ast->children[ast->children.size() - 3].get(), bodyBuilder);

	// 
	std::unordered_map<String, VariableValue*> final_symbol_meaning_loop;
	for (auto& [symbol, initValue] : initial_symbol_meaning_loop)
	{
		auto loopValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto innerValue = dynamic_cast_operation_or_result_value(loopValue))
		{
			loopValue = callInnerBuilder.createResultValue(innerValue, symbol);
			auto phiValue = loopBuilder.createPhiValue(initValue->getType(), initValue, loopValue, symbol);
			final_symbol_meaning_loop.emplace(symbol, phiValue);
		}
		else
		{
			final_symbol_meaning_loop.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();
	for (auto param : body->getParameterVector())
	{
		auto arg = final_symbol_meaning_loop.at(param->getName());
		callInnerBuilder.setArgument(param->getIndex(), arg);
	}
	for (const auto& [symbol, value] : final_symbol_meaning_loop)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}

	// 
	assertTokenNode(ast->children[ast->children.size() - 2].get(), Token::Category::END);
	assertTokenNode(ast->children[ast->children.size() - 1].get(), Token::Category::DO);
	IterateLoop::CallBuilder callLoopBuilder;
	auto call_loop = callLoopBuilder.createInstance(loop);
	callLoopBuilder.setBegin(getBeginVariable(head, tail, step, outerBuilder));
	callLoopBuilder.setTimes(getTimesVariable(head, tail, step, outerBuilder));
	callLoopBuilder.setStep(step);
	outerBuilder.addSectionCall(callLoopBuilder);
	std::unordered_map<String, VariableValue*> final_symbol_meaning_outer;
	for (auto& [symbol, initValue] : initial_symbol_meaning_outer)
	{
		auto finalValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto phiValue = dynamic_cast<PhiValue*>(finalValue))
		{
			auto resultValue = callLoopBuilder.createResultValue(phiValue, symbol);
			final_symbol_meaning_outer.emplace(symbol, resultValue);
		}
		else
		{
			final_symbol_meaning_outer.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();
	for (auto param : loop->getParameterVector())
	{
		auto arg = initial_symbol_meaning_outer.at(param->getName());
		callLoopBuilder.setArgument(param->getIndex(), arg);
	}
	for (const auto& [symbol, value] : final_symbol_meaning_outer)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}
	return success;
}

bool SectionConstructor::runOnForStatement(AST* ast, SimpleSection::Builder& outerBuilder)
{
	assert(ast->category == AST::Category::FOR_STATEMENT);
	assert(ast->children.size() >= 5);
	assertTokenNode(ast->children[0].get(), Token::Category::FOR);

	// 
	size_t index = 1;
	bool success = true;
	std::vector<String> counterNameVector;
	std::vector<Value*> headVector;
	std::vector<Value*> tailVector;
	std::vector<Value*> stepVector;
	while (ast->children[index]->category == AST::Category::LOOP_INDEX)
	{
		auto index_ast = ast->children[index].get();
		assert(index_ast->children.size() == 4 || index_ast->children.size() == 5);
		assert(index_ast->children[0]->category == AST::Category::LOOP_VARIABLE);
		auto counterName = context->getString(index_ast->children[0]->firstToken.text);
		counterNameVector.push_back(counterName);
		assertTokenNode(index_ast->children[1].get(), Token::Category::ASG);
		assert(index_ast->children[2]->category == AST::Category::LOOP_HEAD);
		auto head = getRightValueOperand(index_ast->children[2]->children[0].get(), outerBuilder);
		headVector.push_back(head);
		assert(index_ast->children[3]->category == AST::Category::LOOP_TAIL);
		auto tail = getRightValueOperand(index_ast->children[3]->children[0].get(), outerBuilder);
		tailVector.push_back(tail);
		Value* step = nullptr;
		if (index_ast->children.size() == 5)
		{
			assert(index_ast->children[4]->category == AST::Category::LOOP_STEP);
			step = getRightValueOperand(index_ast->children[4]->children[0].get(), outerBuilder);
		}
		else
		{
			step = defaultLoopStep;
		}
		stepVector.push_back(step);
		success = success && head != nullptr && tail != nullptr && step != nullptr;
		index++;
	}
	assert(index == ast->children.size() - 3);
		
	// 
	auto initial_symbol_meaning_outer = symbolTable.getTopVariable();
	size_t counterNum = index - 1;
	ParallelLoop::Builder loopBuilder;
	auto loop = loopBuilder.createInstance(context, counterNum);
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_outer)
	{
		auto param = loopBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}
	for (size_t i = 0; i < counterNum; i++)
	{
		symbolTable.defineSymbol(counterNameVector[i], ValueDefinition{ValueDefinition::Position::VARIABLE, loop->getCounter(i)});
	}

	// 
	auto initial_symbol_meaning_loop = symbolTable.getTopVariable();
	SimpleSection::Builder bodyBuilder;
	auto body = bodyBuilder.createInstance(context);
	symbolTable.pushLayer();
	for (const auto& [symbol, arg] : initial_symbol_meaning_loop)
	{
		auto param = bodyBuilder.createParameter(arg->getType(), symbol);
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, param });
	}

	// 
	assert(ast->children[ast->children.size() - 3]->category == AST::Category::LOOP_BODY);
	SectionCall::Builder callInnerBuilder;
	callInnerBuilder.createInstance(body);
	loopBuilder.setBodyCall(callInnerBuilder);
	runOnSimpleRegionPushLayer(ast->children[ast->children.size() - 3].get(), bodyBuilder);

	// 
	std::unordered_map<String, VariableValue*> final_symbol_meaning_loop;
	for (auto& [symbol, initValue] : initial_symbol_meaning_loop)
	{
		auto loopValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto innerValue = dynamic_cast_operation_or_result_value(loopValue))
		{
			loopValue = callInnerBuilder.createResultValue(innerValue, symbol);
			auto phiValue = loopBuilder.createPhiValue(initValue->getType(), initValue, loopValue, symbol);
			final_symbol_meaning_loop.emplace(symbol, phiValue);
		}
		else
		{
			final_symbol_meaning_loop.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();
	for (auto param : body->getParameterVector())
	{
		auto arg = final_symbol_meaning_loop.at(param->getName());
		if (auto phiValue = dynamic_cast<PhiValue*>(arg))
		{
			// 
			callInnerBuilder.setArgument(param->getIndex(), phiValue->getIncoming(0));
		}
		else
		{
			callInnerBuilder.setArgument(param->getIndex(), arg);
		}
	}
	for (const auto& [symbol, value] : final_symbol_meaning_loop)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}

	// 
	assertTokenNode(ast->children[ast->children.size() - 2].get(), Token::Category::END);
	assertTokenNode(ast->children[ast->children.size() - 1].get(), Token::Category::FOR);
	ParallelLoop::CallBuilder callLoopBuilder;
	callLoopBuilder.createInstance(loop);
	outerBuilder.addSectionCall(callLoopBuilder);
	std::unordered_map<String, VariableValue*> final_symbol_meaning_outer;
	for (auto& [symbol, initValue] : initial_symbol_meaning_outer)
	{
		auto finalValue = std::get<ValueDefinition>(symbolTable.findSymbol(symbol)).value;
		if (auto phiValue = dynamic_cast<PhiValue*>(finalValue))
		{
			auto resultValue = callLoopBuilder.createResultValue(phiValue, symbol);
			final_symbol_meaning_outer.emplace(symbol, resultValue);
		}
		else
		{
			final_symbol_meaning_outer.emplace(symbol, initValue);
		}
	}
	symbolTable.popLayer();
	for (auto param : loop->getParameterVector())
	{
		auto arg = initial_symbol_meaning_outer.at(param->getName());
		callLoopBuilder.setArgument(param->getIndex(), arg);
	}
	for (const auto& [symbol, value] : final_symbol_meaning_outer)
	{
		symbolTable.updateSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, value });
	}
	return success;
}

bool SectionConstructor::runOnVariableDefinition(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->category == AST::Category::VARIABLE_DEFINITION);
	assert(ast->children.size() == 4);
	assertTokenNode(ast->children[0].get(), Token::Category::VAR);
	assertTokenNode(ast->children[1].get(), Token::Category::IDENTIFIER);
	assertTokenNode(ast->children[2].get(), Token::Category::ASG);

	auto symbol = context->getString(ast->children[1]->firstToken.text);
	checkRedefinition(symbol, ast->children[1]->firstToken.line, ast->children[1]->firstToken.column);
	auto initValue = getRightValueOperand(ast->children[3].get(), sectionBuilder);
	if (initValue == nullptr)
	{
		errorHandler.info("Duplicated defination of variable `" + symbol.str() + "`: invalid initialization", ast->children[3]->firstToken.line, ast->children[3]->firstToken.column);
		return false;
	}
	auto copyValue = sectionBuilder.createCopyValue(initValue, symbol);
	symbolTable.defineSymbol(symbol, ValueDefinition{ ValueDefinition::Position::VARIABLE, copyValue });
	return true;
}

bool SectionConstructor::runOnConstantDefinition(AST* ast)
{
	assert(ast->category == AST::Category::CONST_DEFINITION);
	assert(ast->children.size() == 4);
	assertTokenNode(ast->children[0].get(), Token::Category::CONST);
	assertTokenNode(ast->children[1].get(), Token::Category::IDENTIFIER);
	assertTokenNode(ast->children[2].get(), Token::Category::ASG);

	auto symbol = context->getString(ast->children[1]->firstToken.text);
	checkRedefinition(symbol, ast->children[1]->firstToken.line, ast->children[1]->firstToken.column);
	auto constantValue = dynamic_cast<ConstantValue*>(getFixedValueOperand(ast->children[3].get()));
	if (constantValue == nullptr)
	{
		errorHandler.info("Assigment of constant `" + symbol.str() + " is invalid", ast->children[3]->firstToken.line, ast->children[3]->firstToken.column);
		return false;
	}
	else
	{
		auto contantSymbol = context->createConstantValue(constantValue->getType(), constantValue->getValue(), symbol);
		symbolTable.defineSymbol(symbol, ValueDefinition{ ValueDefinition::Position::CONSTANT, contantSymbol });
	}
	return true;
}

bool SectionConstructor::runOnStructDefinition(AST* ast)
{
	assert(ast->category == AST::Category::STRUCT_DEFINITION);
	assert(ast->children.size() == 5);
	assertTokenNode(ast->children[0].get(), Token::Category::STRUCT);
	assertTokenNode(ast->children[1].get(), Token::Category::IDENTIFIER);
	assertTokenNode(ast->children[3].get(), Token::Category::END);
	assertTokenNode(ast->children[4].get(), Token::Category::STRUCT);

	auto symbol = context->getString(ast->children[1]->firstToken.text);
	checkRedefinition(symbol, ast->children[1]->firstToken.line, ast->children[1]->firstToken.column);
	StructDefinition def;
	if (!runOnStructBody(ast->children[2].get(), def))
	{
		return false;
	}
	std::vector<Type> elemTypes(def.initValues.size());
	std::transform(def.initValues.begin(), def.initValues.end(), elemTypes.begin(), [](Value* value) {
		return value->getType();
	});
	def.type = context->getStructType(symbol, elemTypes, def.elemNames);
	symbolTable.defineSymbol(symbol, def);
	return true;
}

bool SectionConstructor::runOnStructBody(AST* ast, StructDefinition& def)
{
	assert(ast->category == AST::Category::STRUCT_BODY);

	bool success = true;
	for (const auto& child : ast->children)
	{
		if (child->category == AST::Category::VARIABLE_DEFINITION)
		{
			assert(child->category == AST::Category::VARIABLE_DEFINITION);
			assert(child->children.size() == 4);
			assertTokenNode(child->children[0].get(), Token::Category::VAR);
			assertTokenNode(child->children[1].get(), Token::Category::IDENTIFIER);
			assertTokenNode(child->children[2].get(), Token::Category::ASG);

			auto symbol = context->getString(child->children[1]->firstToken.text);
			auto initValue = getFixedValueOperand(child->children[3].get());
			if (initValue == nullptr)
			{
				errorHandler.info("Initialization of structure member `" + symbol.str() + "`: invalid initialization", child->children[3]->firstToken.line, child->children[3]->firstToken.column);
				success = false;
			}
			def.elemNames.push_back(context->getString(child->children[1]->firstToken.text));
			def.initValues.push_back(initValue);
		}
		else
		{
			errorHandler.error("Expecting structure member defination", child->firstToken.line, child->firstToken.column);
			success = false;
		}
	}
	return success;
}

bool SectionConstructor::runOnAssignStatement(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->category == AST::Category::ASSIGN_STATEMENT);
	//assert(ast->children.size() == 3);

	// 
	;
	//auto leftOperand = getLeftValueOperand(ast->children[0].get(), sectionBuilder, leftSymbol, leftValueDefinition);
	std::vector<String> leftSymbols;
	std::vector<ValueDefinition> leftValueDefinitions;
	std::vector<Value*> leftOperands;
	for (size_t i = 0; i < ast->children.size() - 2; i++)
	{
		if (i % 2 == 0)
		{
			leftOperands.push_back(getLeftValueOperand(ast->children[i].get(), sectionBuilder,
				leftSymbols.emplace_back(), leftValueDefinitions.emplace_back()));
		}
		else
		{
			assertTokenNode(ast->children[i].get(), Token::Category::COMMA);
		}
	}
	std::vector<Value*> rightOperands;
	if (leftOperands.size() == 1)
	{
		rightOperands.push_back(getRightValueOperand(ast->children.back().get(), sectionBuilder));
	}
	else
	{
		rightOperands = runOnMultipleReturnCall(ast->children.back().get(), sectionBuilder);
	}
	if (std::find(leftOperands.begin(), leftOperands.end(), nullptr) != leftOperands.end() ||
		std::find(rightOperands.begin(), rightOperands.end(), nullptr) != rightOperands.end())
	{
		return false;
	}

	// Operation
	for (size_t i = 0; i < leftOperands.size(); i++)
	{
		auto leftOperand = leftOperands[i];
		auto leftSymbol = leftSymbols[i];
		auto leftValueDefinition = leftValueDefinitions[i];
		auto rightOperand = rightOperands[i];
		Operation leftCast;
		Operation rightCast;
		auto assignOperation = matchOperatorExpression(
			ast->children[ast->children.size() - 2]->firstToken, context,
			leftOperand->getType(), rightOperand->getType(),
			false, isImmediateConstant(rightOperand),
			leftCast, rightCast);
		if (assignOperation == nullptr)
		{
			return false;
		}
		assert(leftCast == nullptr);

		// OperationValue
		if (rightCast != nullptr)
		{
			rightOperand = sectionBuilder.createOperationValue(rightCast, { rightOperand });
		}
		Value* assignResult = nullptr;
		auto leftName = leftOperand == leftValueDefinition.value ? leftSymbol : String();
		assignResult = sectionBuilder.createOperationValue(assignOperation, { rightOperand }, leftName);
		while (leftOperand != leftValueDefinition.value)
		{
			auto operationValue = dynamic_cast<OperationValue*>(leftOperand);
			assert(operationValue != nullptr);
			if (operationValue->getOperation().getName().equals("array.getAt"))
			{
				auto arrayType = operationValue->getOperation().getParameterType(0).cast<ArrayType>();
				assert(arrayType != nullptr);
				auto srcVector(operationValue->getSrcVector());
				srcVector.push_back(assignResult);
				leftOperand = srcVector[0];
				leftName = leftOperand == leftValueDefinition.value ? leftSymbol : String();
				assignResult = sectionBuilder.createOperationValue(context->getArraySetOperation(arrayType), srcVector, leftName);
			}
			else
			{
				assert(false);
			}
		}
		leftValueDefinition.value = assignResult;
		symbolTable.updateSymbol(leftSymbol, leftValueDefinition);
	}
	return true;
}

Value* SectionConstructor::runOnUnaryExpression(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->children.size() == 2);

	// 
	auto rightOperand = getRightValueOperand(ast->children[1].get(), sectionBuilder);

	// Operation
	if (rightOperand == nullptr)
	{
		return nullptr;
	}
	Operation leftCast;
	Operation rightCast;
	auto assignOperation = matchOperatorExpression(
		ast->children[0]->firstToken, context,
		Type(), rightOperand->getType(),
		false, isImmediateConstant(rightOperand),
		leftCast, rightCast);
	assert(leftCast == nullptr);
	if (assignOperation == nullptr)
	{
		return nullptr;
	}

	// 3.OperationValue
	if (rightCast != nullptr)
	{
		rightOperand = sectionBuilder.createOperationValue(rightCast, { rightOperand });
	}
	return sectionBuilder.createOperationValue(assignOperation, { rightOperand });
}

Value* SectionConstructor::runOnBinaryExpression(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->children.size() == 3);

	// 
	auto leftOperand = getRightValueOperand(ast->children[0].get(), sectionBuilder);
	auto rightOperand = getRightValueOperand(ast->children[2].get(), sectionBuilder);

	// Operation
	if (leftOperand == nullptr || rightOperand == nullptr)
	{
		return nullptr;
	}
	Operation leftCast;
	Operation rightCast;
	auto assignOperation = matchOperatorExpression(
		ast->children[1]->firstToken, context,
		leftOperand->getType(), rightOperand->getType(),
		isImmediateConstant(leftOperand), isImmediateConstant(rightOperand),
		leftCast, rightCast);
	if (assignOperation == nullptr)
	{
		return nullptr;
	}

	// 3.OperationValue
	if (leftCast != nullptr)
	{
		leftOperand = sectionBuilder.createOperationValue(leftCast, { leftOperand });
	}
	if (rightCast != nullptr)
	{
		rightOperand = sectionBuilder.createOperationValue(rightCast, { rightOperand });
	}
	return sectionBuilder.createOperationValue(assignOperation, { leftOperand, rightOperand });
}

Value* SectionConstructor::runOnSingleReturnCall(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->children.size() == 2);
	assert(ast->children[1]->category == AST::Category::ARGUMENT_LIST);

	auto symbol = context->getString(ast->firstToken.text);
	auto meaning = symbolTable.findSymbol(symbol);
	auto args = getArgumentList(ast->children[1].get(), sectionBuilder);
	if (std::find(args.begin(), args.end(), nullptr) != args.end())
	{
		return nullptr;
	}
	if (auto def = std::get_if<Type>(&meaning))
	{
		// 
		auto type = getTypeObject(ast->children[0].get());
		switch (args.size())
		{
		case 0:
			return context->createInvalidValue(type, symbol);
		case 1: {
			bool isBasicType = false;
			if (args[0]->getType().canBeArgument(type))
			{
				// 
				return sectionBuilder.createCopyValue(args[0]);
			}
			auto constructOperation = matchBasicTypeConstructor(context, isBasicType,
				type, args[0]->getType(), ast->firstToken.line, ast->firstToken.column);
			if (constructOperation != nullptr)
			{
				return sectionBuilder.createOperationValue(constructOperation, args);
			}
			assert(false);
			break;
		}
		default:
			assert(false);
		}
	}
	else if (auto def = std::get_if<FunctionDefinition>(&meaning))
	{
		if (def->returnValues.size() != 1)
		{
			errorHandler.error("Function `" + symbol.str() + " is not a return value", ast->firstToken.line, ast->firstToken.column);
			return nullptr;
		}
		auto returnValue = def->returnValues.front();
		if (auto returnVariableValue = dynamic_cast<VariableValue*>(returnValue))
		{
			auto callee = def->section;
			assert(returnVariableValue->getSection() == callee);
			SectionCall::Builder callBuilder;
			callBuilder.createInstance(callee);
			checkClosureCall(callee, args);
			for (size_t i = 0; i < callee->getParameterVectorSize(); i++)
			{
				if (args[i]->getType().canBeArgument(callee->getParameter(i)->getType()))
				{
					callBuilder.setArgument(i, args[i]);
				}
				else
				{
					errorHandler.error("Mismatch between argument type and parameter type", ast->firstToken.line, ast->firstToken.column);
					return nullptr;
				}
			}
			sectionBuilder.addSectionCall(callBuilder);
			return callBuilder.createResultValue(returnVariableValue, returnVariableValue->getName());
		}
		return returnValue;
	}
	else
	{
		std::vector<Type> argType(args.size());
		std::transform(args.begin(), args.end(), argType.begin(), [](Value* arg) {
			return arg->getType();
		});
		std::vector<bool> isImm(args.size());
		std::transform(args.begin(), args.end(), isImm.begin(), [](Value* arg) {
			return isImmediateConstant(arg);
		});
		auto op = matchIntrinsicFunction(ast->firstToken, context, argType, isImm);
		if (op != nullptr)
		{
			return sectionBuilder.createOperationValue(op, args);
		}
	}
	errorHandler.error("Symbol `" + symbol.str() + "` is not a function", ast->firstToken.line, ast->firstToken.column);
	return nullptr;
}

std::vector<Value*> SectionConstructor::runOnMultipleReturnCall(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->children.size() == 2);
	assertTokenNode(ast->children[0].get(), Token::Category::IDENTIFIER);
	auto args = getArgumentList(ast->children[1].get(), sectionBuilder);

	auto symbol = context->getString(ast->children[0]->firstToken.text);
	auto meaning = symbolTable.findSymbol(symbol);
	if (auto def = std::get_if<FunctionDefinition>(&meaning))
	{
		auto callee = def->section;
		SectionCall::Builder callBuilder;
		callBuilder.createInstance(callee);
		for (size_t i = 0; i < callee->getParameterVectorSize(); i++)
		{
			if (args[i]->getType().canBeArgument(callee->getParameter(i)->getType()))
			{
				callBuilder.setArgument(i, args[i]);
			}
		}
		sectionBuilder.addSectionCall(callBuilder);

		auto returnValues(def->returnValues);
		for (auto& returnValue : returnValues)
		{
			if (auto returnVariableValue = dynamic_cast<VariableValue*>(returnValue))
			{
				assert(returnVariableValue->getSection() == callee);
				returnValue = callBuilder.createResultValue(returnVariableValue, symbol);
			}
		}
		return returnValues;
	}
	errorHandler.error("Symbol `" + symbol.str() + "` is not a function", ast->firstToken.line, ast->firstToken.column);
	return {};
}

Value* SectionConstructor::runOnArrayElement(AST* ast, SimpleSection::Builder& sectionBuilder,
	bool isLeft, String& symbol, ValueDefinition& valueDefinition)
{
	assert(ast->children.size() == 2);
	assert(ast->children[1]->category == AST::Category::ARRAY_SUBSCRIPT);

	Value* baseValue = nullptr;
	if (isLeft)
	{
		baseValue = getLeftValueOperand(ast->children[0].get(), sectionBuilder, symbol, valueDefinition);
	}
	else
	{
		baseValue = getRightValueOperand(ast->children[0].get(), sectionBuilder);
	}
	auto baseType = baseValue->getType().cast<ArrayType>();
	if (baseType == nullptr)
	{
		errorHandler.error("Subscribing non-array type", ast->firstToken.line, ast->firstToken.column);
		return nullptr;
	}

	// TODO: 
	auto indexVector = getArraySubscript(ast->children[1].get(), sectionBuilder);
	if (std::find(indexVector.begin(), indexVector.end(), nullptr) != indexVector.end())
	{
		errorHandler.info("Invalid array index", ast->firstToken.line, ast->firstToken.column);
		return nullptr;
	}
	return sectionBuilder.createArrayGetValue(baseValue, indexVector);
}

Value* SectionConstructor::runOnStructElement(AST* ast, SimpleSection::Builder& sectionBuilder,
	bool isLeft, String& symbol, ValueDefinition& valueDefinition)
{
	assert(ast->children.size() == 3);
	assertTokenNode(ast->children[1].get(), Token::Category::DOT);
	assertTokenNode(ast->children[2].get(), Token::Category::IDENTIFIER);

	Value* baseValue = nullptr;
	if (isLeft)
	{
		baseValue = getLeftValueOperand(ast->children[0].get(), sectionBuilder, symbol, valueDefinition);
	}
	else
	{
		baseValue = getRightValueOperand(ast->children[0].get(), sectionBuilder);
	}
	auto baseType = baseValue->getType().cast<StructType>();
	if (baseType == nullptr)
	{
		errorHandler.error("Accessing member of non-structuer type", ast->firstToken.line, ast->firstToken.column);
		return nullptr;
	}

	auto elem = context->getString(ast->children[2]->firstToken.text);
	auto result = sectionBuilder.createStructGetValue(baseValue, elem);
	if (result == nullptr)
	{
		errorHandler.error(std::string("Member not found") + elem.str(), ast->children[2]->firstToken.line, ast->children[2]->firstToken.column);
	}
	return result;
}

Value* SectionConstructor::runOnTupleInitializer(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	// TODO
	return nullptr;
}

Type SectionConstructor::getTypeObject(AST* ast)
{
	switch (ast->category)
	{
	case AST::Category::TERMINAL: {
		assert(ast->firstToken.category == Token::Category::IDENTIFIER);
		auto meaning = symbolTable.findSymbol(context->getString(ast->firstToken.text));
		if (auto p = std::get_if<Type>(&meaning))
		{
			return *p;
		}
		if (auto p = std::get_if<StructDefinition>(&meaning))
		{
			return p->type;
		}
		errorHandler.error("Symbol `" + std::string(ast->firstToken.text) +  "` is not a type name", ast->firstToken.line, ast->firstToken.column);
		return Type();
	}
	case AST::Category::ARRAY_ELEMENT: {
		auto elementType = getTypeObject(ast->children[0].get());
		// TODO: 
		const auto& dimVector = ast->children[1]->children;
		auto n = std::accumulate(dimVector.begin(), dimVector.end(), size_t(1), [](size_t n, const std::unique_ptr<AST>& ast) {
			return n + size_t(ast->firstToken.category == Token::Category::COMMA ? 1 : 0);
		});
		return context->getArrayType(elementType, n);
	}
	default:
		assert(false);
	}
	return Type();
}

Value* SectionConstructor::getLeftValueOperand(AST* ast, SimpleSection::Builder& sectionBuilder, String& symbol, ValueDefinition& valueDefinition)
{
	switch (ast->category)
	{
	case AST::Category::TERMINAL: {
		switch (ast->firstToken.category)
		{
		case Token::Category::IDENTIFIER: {
			symbol = context->getString(ast->firstToken.text);
			auto meaning = symbolTable.findSymbol(symbol);
			if (auto def = std::get_if<ValueDefinition>(&meaning))
			{
				valueDefinition = *def;
				if (valueDefinition.position == ValueDefinition::Position::CONSTANT)
				{
					errorHandler.error("Assigning constant", ast->firstToken.line, ast->firstToken.column);
				}
				else
				{
					// 
					return def->value;
				}
			}
			errorHandler.error("Symbol `" + symbol.str() + " is not a variable", ast->firstToken.line, ast->firstToken.column);
			break;
		}
		default:
			assert(false);
		}
		break;
	}
	case AST::Category::PRIMARY_EXPRESSION:
		assert(ast->children.size() == 3);
		assertTokenNode(ast->children[0].get(), Token::Category::LPAREN);
		assertTokenNode(ast->children[2].get(), Token::Category::RPAREN);
		return getLeftValueOperand(ast->children[1].get(), sectionBuilder, symbol, valueDefinition);
		break;
	case AST::Category::ARRAY_ELEMENT:
		return runOnArrayElement(ast, sectionBuilder, true, symbol, valueDefinition);
	case AST::Category::STRUCT_ELEMENT:
		return runOnStructElement(ast, sectionBuilder, true, symbol, valueDefinition);
	default:
		assert(false);
	}
	return nullptr;
}

Value* SectionConstructor::getRightValueOperand(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	String symbol;
	ValueDefinition valueDefinition;
	switch (ast->category)
	{
	case AST::Category::TERMINAL: {
		switch (ast->firstToken.category)
		{
		case Token::Category::IDENTIFIER: {
			symbol = context->getString(ast->firstToken.text);
			auto meaning = symbolTable.findSymbol(symbol);
			if (auto def = std::get_if<ValueDefinition>(&meaning))
			{
				// 
				if (auto var = dynamic_cast<VariableValue*>(def->value))
				{
					return checkClosureParam(var, sectionBuilder, ast->firstToken.line, ast->firstToken.column);
				}
				return def->value;
			}
			errorHandler.error("Symbol `" + symbol.str() + "` is not constant or variable", ast->firstToken.line, ast->firstToken.column);
			break;
		}
		case Token::Category::DEC_INTEGER:
			return context->createConstantValue(
				context->getIntegerType(),
				context->getInteger(big_int(ast->firstToken.text)));
		case Token::Category::DEC_RATIONAL:
			return context->createConstantValue(
				context->getRealType(), // TODO: 
				context->getDecimal(big_decimal(ast->firstToken.text)));
		default:
			assert(false);
		}
		break;
	}
	case AST::Category::PRIMARY_EXPRESSION:
		assert(ast->children.size() == 3);
		assertTokenNode(ast->children[0].get(), Token::Category::LPAREN);
		assertTokenNode(ast->children[2].get(), Token::Category::RPAREN);
		return getRightValueOperand(ast->children[1].get(), sectionBuilder);
	case AST::Category::UNARY_EXPRESSION:
		return runOnUnaryExpression(ast, sectionBuilder);
	case AST::Category::BINARY_EXPRESSION:
		return runOnBinaryExpression(ast, sectionBuilder);
	case AST::Category::FUNCTION_CALL:
		return runOnSingleReturnCall(ast, sectionBuilder);
	case AST::Category::ARRAY_ELEMENT:
		return runOnArrayElement(ast, sectionBuilder, false, symbol, valueDefinition);
	case AST::Category::STRUCT_ELEMENT:
		return runOnStructElement(ast, sectionBuilder, false, symbol, valueDefinition);
	case AST::Category::TUPLE_INITIALIZER:
		return runOnTupleInitializer(ast, sectionBuilder);
	default:
		assert(false);
	}
	return nullptr;
}

Value* SectionConstructor::getFixedValueOperand(AST* ast)
{
	switch (ast->category)
	{
	case AST::Category::TERMINAL: {
		switch (ast->firstToken.category)
		{
		case Token::Category::IDENTIFIER: {
			auto symbol = context->getString(ast->firstToken.text);
			auto meaning = symbolTable.findSymbol(symbol);
			if (auto def = std::get_if<ValueDefinition>(&meaning))
			{
				if (def->position == ValueDefinition::Position::CONSTANT)
				{
					// 
					assert(dynamic_cast<ConstantValue*>(def->value) != nullptr);
					return def->value;
				}
			}
			errorHandler.error("Symbol `" + symbol.str() + " is not constant", ast->firstToken.line, ast->firstToken.column);
			break;
		}
		case Token::Category::DEC_INTEGER:
			return context->createConstantValue(
				context->getIntegerType(),
				context->getInteger(big_int(ast->firstToken.text)));
		case Token::Category::DEC_RATIONAL:
			return context->createConstantValue(
				context->getRealType(), // TODO: 
				context->getDecimal(big_decimal(ast->firstToken.text)));
		default:
			assert(false);
		}
		break;
	}
	case AST::Category::RIGHT_VALUE_EXPRESSION: {
		// 
		SimpleSection::Builder sectionBuilder;
		auto section = sectionBuilder.createInstance(context);
		auto value = getRightValueOperand(ast, sectionBuilder);
		ConstantFoldingDeepPass pass;
		pass.load({ section });
		pass.run();
		pass.result.validate();
		return pass.mapValue(value);
	}
	default:
		assert(false);
	}
	return nullptr;
}

std::vector<Value*> SectionConstructor::getArgumentList(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->category == AST::Category::ARGUMENT_LIST);
	std::vector<Value*> result;
	for (size_t i = 0; i < ast->children.size(); i++)
	{
		switch (ast->children[i]->category)
		{
		case AST::Category::TERMINAL:
			switch (ast->children[i]->firstToken.category)
			{
			case Token::Category::LPAREN:
				assert(i == 0);
				break;
			case Token::Category::RPAREN:
				assert(i == ast->children.size() - 1);
				break;
			case Token::Category::COMMA:
				assert(i != 0 && i % 2 == 0);
				break;
			case Token::Category::IDENTIFIER:
			case Token::Category::CONSTANT_NUMERAL:
				assert(i % 2 == 1);
				result.emplace_back(getRightValueOperand(ast->children[i].get(), sectionBuilder));
				break;
			default:
				assert(false);
			}
			break;
		case AST::Category::RIGHT_VALUE_EXPRESSION:
			assert(i % 2 == 1);
			result.emplace_back(getRightValueOperand(ast->children[i].get(), sectionBuilder));
			break;
		default:
			assert(false);
		}
	}
	return result;
}

std::vector<Value*> SectionConstructor::getArraySubscript(AST* ast, SimpleSection::Builder& sectionBuilder)
{
	assert(ast->category == AST::Category::ARRAY_SUBSCRIPT);
	std::vector<Value*> result;
	for (size_t i = 0; i < ast->children.size(); i++)
	{
		switch (ast->children[i]->category)
		{
		case AST::Category::TERMINAL:
			switch (ast->children[i]->firstToken.category)
			{
			case Token::Category::LBRACKET:
				assert(i == 0);
				break;
			case Token::Category::RBRACKET:
				assert(i == ast->children.size() - 1);
				break;
			case Token::Category::COMMA:
				assert(i != 0 && i % 2 == 0);
				break;
			case Token::Category::IDENTIFIER:
			case Token::Category::CONSTANT_NUMERAL:
				assert(i % 2 == 1);
				result.emplace_back(getRightValueOperand(ast->children[i].get(), sectionBuilder));
				break;
			default:
				assert(false);
			}
			break;
		case AST::Category::RIGHT_VALUE_EXPRESSION:
			assert(i % 2 == 1);
			result.emplace_back(getRightValueOperand(ast->children[i].get(), sectionBuilder));
			break;
		default:
			assert(false);
		}
	}
	return result;
}

Value* SectionConstructor::getBeginVariable(Value* head, Value* tail, Value* step, SimpleSection::Builder& sectionBuilder)
{
	// begin = select(step > 0, head, select(step < 0, tail, invalid))
	if (auto step_numeral = dynamic_cast<ConstantValue*>(step))
	{
		auto step_value = step_numeral->getValue<Integer>();
		auto step_value_sign = step_value.sign();
		if (step_value_sign > 0)
		{
			return head;
		}
		if (step_value_sign < 0)
		{
			return tail;
		}
		return context->createInvalidValue(context->getIntegerType());
	}
	Operation::Data opData;
	opData.parameterTypes = { context->getIntegerType(), context->getIntegerType() };
	opData.returnType = context->getLogicType();
	opData.name = context->getString("Integer.gt");
	auto op_gt = context->getOperation(opData);
	opData.name = context->getString("Integer.lt");
	auto op_lt = context->getOperation(opData);
	auto value_gt = sectionBuilder.createOperationValue(op_gt, { step, context->getZeroValue() });
	auto value_lt = sectionBuilder.createOperationValue(op_lt, { step, context->getZeroValue() });
	auto select2 = sectionBuilder.createSelectValue(context->getIntegerType(),
		value_lt, tail, context->createInvalidValue(context->getIntegerType()));
	auto select1 = sectionBuilder.createSelectValue(context->getIntegerType(),
		value_gt, head, select2);
	return select1;
}

Value* SectionConstructor::getTimesVariable(Value* head, Value* tail, Value* step, SimpleSection::Builder& sectionBuilder)
{
	Integer step_value;
	if (auto step_numeral = dynamic_cast<ConstantValue*>(step))
	{
		step_value = step_numeral->getValue<Integer>();
		if (step_value.sign() == 0)
		{
			errorHandler.error("stride");
			return nullptr;
		}
		if (auto head_numeral = dynamic_cast<ConstantValue*>(head))
		{
			if (auto tail_numeral = dynamic_cast<ConstantValue*>(tail))
			{
				auto head_value = head_numeral->getValue<Integer>();
				auto tail_value = tail_numeral->getValue<Integer>();
				auto times_value = (tail_value - head_value) / step_value + context->getInteger(1);
				if (times_value.sign() < 0)
				{
					times_value = context->getZero();
				}
				return context->createConstantValue(context->getIntegerType(), times_value);
			}
		}
	}

	// times = max(0, (tail - head) / step + 1)
	Operation::Data opData;
	opData.parameterTypes = { context->getIntegerType(), context->getIntegerType() };
	opData.returnType = context->getIntegerType();
	opData.name = context->getString("Integer.sub");
	auto op_sub = context->getOperation(opData);
	auto value_sub = sectionBuilder.createOperationValue(op_sub, { tail, head });
	OperationValue* value_div = nullptr;
	if (step_value != nullptr && step_value.getData() == 1)
	{
		value_div = value_sub;
	}
	else if (step_value != nullptr && step_value.getData() == -1)
	{
		value_div = sectionBuilder.createOperationValue(op_sub, { head, tail });
	}
	else
	{
		opData.name = context->getString("Integer.div");
		auto op_div = context->getOperation(opData);
		value_div = sectionBuilder.createOperationValue(op_div, { value_sub, step });
	}
	opData.name = context->getString("Integer.add");
	auto op_add = context->getOperation(opData);
	auto one = context->createConstantValue(context->getIntegerType(), context->getInteger(1));
	auto value_add = sectionBuilder.createOperationValue(op_add, { value_div, one });
	opData.name = context->getString("Integer.max");
	auto op_max = context->getOperation(opData);
	auto value_max = sectionBuilder.createOperationValue(op_max, { context->getZeroValue(), value_add });
	return value_max;
}

void SectionConstructor::checkRedefinition(String symbol, size_t line, size_t column)
{
	if (!allowRedefinition)
	{
		size_t layer = 0;
		auto meaning = symbolTable.findSymbol(symbol, layer);
		if (meaning.index() != 0 && layer == symbolTable.getCurrentLayer())
		{
			errorHandler.error("Duplicated defination of variable `" + symbol.str() + "`", line, column);
		}
	}
}

Value* SectionConstructor::checkClosureParam(VariableValue* value, SimpleSection::Builder& sectionBuilder, size_t line, size_t column)
{
	auto outer_section = value->getSection();
	auto inner_section = sectionBuilder.getInstance();
	auto symbol = inner_section->getName();
	auto meaning = symbolTable.findSymbol(symbol);
	if (outer_section != inner_section)
	{
		// Section
		if (auto def = std::get_if<FunctionDefinition>(&meaning))
		{
			auto result = def->closureSymbols.try_emplace(value->getName(), inner_section->getParameterVectorSize());
			InputValue* param = nullptr;
			if (result.second)
			{
				InputValue::Builder paramBuilder;
				param = paramBuilder.createInstance(context);
				paramBuilder.copyType(value);
				paramBuilder.copyName(value);
				sectionBuilder.addParameter(paramBuilder);
			}
			else
			{
				param = inner_section->getParameter(result.first->second);
			}
			symbolTable.updateSymbol(symbol, meaning);
			return param;
		}
		errorHandler.error("Closure must be function", line, column);
		return nullptr;
	}
	return value;
}

void SectionConstructor::checkClosureCall(Section* callee, std::vector<Value*>& args)
{
	auto symbol = callee->getName();
	auto meaning = symbolTable.findSymbol(symbol);
	if (auto def = std::get_if<FunctionDefinition>(&meaning))
	{
		if (!def->closureSymbols.empty())
		{
			args.resize(callee->getParameterVectorSize() + def->closureSymbols.size());
			for (const auto& [name, index] : def->closureSymbols)
			{
				args[index] = std::get<ValueDefinition>(symbolTable.findSymbol(name)).value;
			}
		}
	}
}
