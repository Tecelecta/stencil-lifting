#define BACKEND_EXPORTS
#include "PrintCodePass.h"

PrintProcedurePass::PrintProcedurePass(std::ostream& out) : PrintCodePass(out)
{
	defaultFunction = [](Value* value, uint32_t i) {};

	visitInputValue = [this](InputValue* value, uint32_t i)
		{
			isInput[i] = true;
		};

	visitOperationValue = [this](OperationValue* value, uint32_t i)
		{
			std::tie(printVertexVector[i].form, printVertexVector[i].op) = getVertexOpFormAndString(value->getOperation());
			if (printVertexVector[i].form == OpForm::ARRAY_SET)
			{
				vertexEquClass[i] = getOperandsByVertex(i)[0];
			}
			printActionVector.push_back({ ActionType::EXPR, i });
		};

	visitResultValue = [this](ResultValue* value, uint32_t i)
		{
			auto call = value->getCall();
			if (std::find(sectionCallVector.begin(), sectionCallVector.end(), call) == sectionCallVector.end())
			{
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size()});
				sectionCallVector.push_back(call);
			}
		};

	opFormAndStringMap["array.getAt"] = { OpForm::ARRAY_GET, "array.getAt"};
	opFormAndStringMap["array.setAt"] = { OpForm::ARRAY_SET, "array.setAt"};
}

void PrintProcedurePass::run(GraphOuterSection src)
{
	setGraph(std::move(src));
	if (getVertexNum() == 0)
	{
		return;
	}
	vertexEquClass.resize(getVertexNum());
	printVertexVector.resize(getVertexNum());
	isInput.resize(vertexSubgraphVector.size(), false);
	isOutput.resize(vertexSubgraphVector.size(), false);
	nextSubgraph = 0;
	if (!iterateSCC(*this))
	{
		throw std::runtime_error("无法生成目标代码");
	}
	printActionVector.push_back({ ActionType::TAIL, getSubgraphNum() - 1 });
	for (const auto& action : printActionVector)
	{
		switch (action.type)
		{
		case ActionType::EXPR:
			printExpr(action.index);
			break;
		case ActionType::HEAD:
			printFunctionHead(subgraphAt(action.index), action.index);
			break;
		case ActionType::TAIL:
			printFunctionTail(subgraphAt(action.index), action.index);
			break;
		case ActionType::CALL:
			printFunctionCall(sectionCallVector[action.index], action.index);
			break;
		case ActionType::IF:
			printBinaryBranchBegin(dynamic_cast<BinaryBranch*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::ELSE:
			printBinaryBranchElse(dynamic_cast<BinaryBranch*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::END_IF:
			printBinaryBranchEnd(dynamic_cast<BinaryBranch*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::DO:
			printIterateLoopBegin(dynamic_cast<IterateLoop*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::END_DO:
			printIterateLoopEnd(dynamic_cast<IterateLoop*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::FOR:
			printParallelLoopBegin(dynamic_cast<ParallelLoop*>(subgraphAt(action.index)), action.index);
			break;
		case ActionType::END_FOR:
			printParallelLoopEnd(dynamic_cast<ParallelLoop*>(subgraphAt(action.index)), action.index);
			break;
		}
	}
}

std::tuple<PrintProcedurePass::OpForm, std::string> PrintProcedurePass::getVertexOpFormAndString(Operation op) const
{
	auto iter = opFormAndStringMap.find(op.getName().str());
	if (iter == opFormAndStringMap.end())
	{
		return { OpForm::FUNCTION, op.getName().str() };
	}
	return iter->second;
}

std::string PrintProcedurePass::getTypeString(Type type) const
{
	return type.toString();
}

std::string PrintProcedurePass::getValueNameString(Value* value) const
{
	std::stringstream ss;
	if (value == nullptr)
	{
		return "__null__";
	}
	if (dynamic_cast<ConstantValue*>(value))
	{
		ss << "c_";
	}
	else
	{
		ss << "v_";
	}
	if (value->getName().isValid())
	{
		ss << value->getName() << '_';
	}
	ss << vertexId(value);
	return ss.str();
}

std::string PrintProcedurePass::getSectionNameString(Section* section) const
{
	if (section == nullptr)
	{
		return "__null__";
	}
	if (section->getName().isValid())
	{
		return section->getName().str();
	}
	std::stringstream ss;
	ss << "f_";
	if (parent == nullptr)
	{
		ss << subgraphId(section);
	}
	else
	{
		ss << parent->subgraphId(section);
	}
	return ss.str();
}

std::string PrintProcedurePass::getExpr(uint32_t i) const
{
	switch (printVertexVector[i].form)
	{
	case OpForm::NUMERAL:
		return getNumeralExpr(i);
	case OpForm::FUNCTION:
		return getCallExpr(i);
	case OpForm::LEFT_UNARY:
		return getLeftUnaryExpr(i);
	case OpForm::RIGHT_UNARY:
		return getRightUnaryExpr(i);
	case OpForm::BINARY:
		return getBinaryExpr(i);
	case OpForm::ARRAY_GET:
		return getArrayGetExpr(i);
	case OpForm::ARRAY_SET:
		return getArraySetExpr(i);
	default:
		return std::string();
	}
}
	 
std::string PrintProcedurePass::getNumeralExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = dynamic_cast<ConstantValue*>(vertexAt(i));
	ss << printVertexVector[i].name << " = " << value->getValue().toString();
	return ss.str();
}

std::string PrintProcedurePass::getCallExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	ss << printVertexVector[i].name << " = " << printVertexVector[i].op << '(';
	const auto& operands = getOperandsByVertex(i);
	for (uint32_t j = 0; j < operands.size(); j++)
	{
		ss << printVertexVector[operands[j]].name;
		if (j != operands.size() - 1)
		{
			ss << ", ";
		}
	}
	ss << ')';
	return ss.str();
}

std::string PrintProcedurePass::getLeftUnaryExpr(uint32_t i) const
{
	std::stringstream ss;
	const auto& operands = getOperandsByVertex(i);
	assert(operands.size() == 1);
	ss << printVertexVector[i].name << " = " << printVertexVector[i].op << printVertexVector[operands[0]].name;
	return ss.str();
}

std::string PrintProcedurePass::getRightUnaryExpr(uint32_t i) const
{
	std::stringstream ss;
	const auto& operands = getOperandsByVertex(i);
	assert(operands.size() == 1);
	ss << printVertexVector[i].name << " = " << printVertexVector[operands[0]].name << printVertexVector[i].op;
	return ss.str();
}

std::string PrintProcedurePass::getBinaryExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	ss << printVertexVector[i].name << " = ";
	const auto& operands = getOperandsByVertex(i);
	for (uint32_t j = 0; j < operands.size(); j++)
	{
		ss << printVertexVector[operands[j]].name;
		if (j != operands.size() - 1)
		{
			ss << printVertexVector[i].op;
		}
	}
	return ss.str();
}

std::string PrintProcedurePass::getArrayGetExpr(uint32_t i) const
{
	return getCallExpr(i);
}

std::string PrintProcedurePass::getArraySetExpr(uint32_t i) const
{
	return getCallExpr(i);
}

void PrintProcedurePass::printExpr(uint32_t i)
{
	auto s = getExpr(i);
	if (!s.empty())
	{
		printTabs(tabNum);
		out << s << '\n';
	}
}

void PrintProcedurePass::printFunctionHead(Section* section, uint32_t i)
{
	out << section->getCategory() << ' ' << getSectionNameString(section) << '(';
	const auto& params = section->getParameterVector();
	for (uint32_t j = 0; j < params.size(); j++)
	{
		out << getTypeString(params[j]->getType()) << ' ' << printVertexVector[vertexId(params[j])].name;
		if (j < params.size() - 1)
		{
			out << ", ";
		}
	}
	out << ")\n";
	tabNum += 1;
}

void PrintProcedurePass::printFunctionTail(Section* section, uint32_t i)
{
	printTabs(--tabNum);
	out << '\n';
}

void PrintProcedurePass::printFunctionCall(SectionCall* call, uint32_t i)
{
	printTabs(tabNum);
	auto callee = call->getCallee();
	const auto results = call->getResultValueVector();
	for (uint32_t j = 0; j < results.size(); j++)
	{
		out << printVertexVector[vertexId(results[j])].name;
		if (j != results.size() - 1)
		{
			out << ", ";
		}
	}
	out << " = " << getSectionNameString(callee) << '(';
	auto args(call->getArgumentVector());
	args.insert(args.end(), call->getSpecializerVector().begin(), call->getSpecializerVector().end());
	for (uint32_t j = 0; j < args.size(); j++)
	{
		out << printVertexVector[vertexId(args[j])].name;
		if (j != args.size() - 1)
		{
			out << ", ";
		}
	}
	out << ")\n";
}

void PrintProcedurePass::printBinaryBranchBegin(BinaryBranch* branch, uint32_t i)
{
	printTabs(++tabNum);
	out << getValueNameString(branch->getCondition()) << '\n';
}

void PrintProcedurePass::printIterateLoopBegin(IterateLoop* loop, uint32_t i)
{
	tabNum += 1;
	printTabs(tabNum);
	out << getValueNameString(loop->getCounter()) << '\n';
	printTabs(tabNum);
	out << getValueNameString(loop->getBegin()) << '\n';
	printTabs(tabNum);
	out << getValueNameString(loop->getTimes()) << '\n';
	printTabs(tabNum);
	out << getValueNameString(loop->getStep()) << '\n';
}

void PrintProcedurePass::printParallelLoopBegin(ParallelLoop* loop, uint32_t i)
{
	tabNum += 1;
	for (size_t n = 0; n < loop->getCounterNum(); n++)
	{
		printTabs(tabNum);
		out << getValueNameString(loop->getCounter(n)) << '\n';
		printTabs(tabNum);
		out << getValueNameString(loop->getPhiValue(0)) << '\n';
	}
}

bool PrintProcedurePass::runOnTrivial(uint32_t i)
{
	auto value = vertexAt(i);
	if (dynamic_cast<VariableValue*>(value) != nullptr)
	{
		auto I = getSubgraphByVertex(i);
		if (nextSubgraph == I)
		{
			nextSubgraph += 1;
			auto section = subgraphAt(I);
			printActionVector.push_back({ ActionType::HEAD, I });
			if (auto branch = dynamic_cast<BinaryBranch*>(section))
			{
				printActionVector.push_back({ ActionType::IF, I });
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
				sectionCallVector.push_back(branch->getIfBranch());
				if (branch->getElseBranch()->getResultValueVectorSize() > 0)
				{
					printActionVector.push_back({ ActionType::ELSE, I });
					printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
					sectionCallVector.push_back(branch->getElseBranch());
				}
				printActionVector.push_back({ ActionType::END_IF, I });
			}
			else if (auto loop = dynamic_cast<IterateLoop*>(section))
			{
				printActionVector.push_back({ ActionType::DO, I });
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
				sectionCallVector.push_back(loop->getBodyCall());
				printActionVector.push_back({ ActionType::END_DO, I });
			}
			else if (auto loop = dynamic_cast<ParallelLoop*>(section))
			{
				printActionVector.push_back({ ActionType::FOR, I });
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
				sectionCallVector.push_back(loop->getBodyCall());
				printActionVector.push_back({ ActionType::END_FOR, I });
			}
		}
		if (parent != nullptr && parent->isReturn.at(parent->vertexId(value)))
		{
			isOutput[i] = true;
		}
	}
	printVertexVector[i].name = getValueNameString(value);
	(*this)(value, i);
	return true;
}

bool PrintProcedurePass::runOnTypical(const vertex_list& scc)
{
	for (uint32_t i : scc)
	{
		auto value = vertexAt(i);
		printVertexVector[i].name = getValueNameString(value);
		if (parent != nullptr && dynamic_cast<VariableValue*>(value) != nullptr && parent->isReturn.at(parent->vertexId(value)))
		{
			isOutput[i] = true;
		}
	}
	return true;
}

void PrintModulePass::run(GraphValueProjection src, const std::vector<Value*>& returnValues)
{
	setGraph(std::move(src));
	AnalyzeArrayBasePass::run();
	isReturn.resize(vertexSubgraphVector.size(), false);
	for (auto value : returnValues)
	{
		if (dynamic_cast<VariableValue*>(value) != nullptr)
		{
			isReturn[vertexId(value)] = true;
		}
	}
	for (uint32_t i = 0; i < isReturn.size(); i++)
	{
		if (vertexFilter.empty() || vertexFilter.at(i))
		{
			if (auto resultValue = dynamic_cast<ResultValue*>(vertexAt(i)))
			{
				for (auto j : getDependencyByVertex(i))
				{
					isReturn[j] = true;
				}
			}
		}
	}
	for (uint32_t i = 0; i < getSubgraphNum(); i++)
	{
		if (subgraphFilter.empty() || subgraphFilter.at(i))
		{
			runOnSubgraph(i);
		}
	}
}
