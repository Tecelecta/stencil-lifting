#define BACKEND_EXPORTS
#include "PrintSdslCodePass.h"

PrintSdslProcedurePass::PrintSdslProcedurePass(std::ostream& out)
	: PrintProcedurePass(out)
{
	visitConstantValue = [this](ConstantValue* value, uint32_t i)
		{
			printVertexVector[i].form = OpForm::NUMERAL;
			printActionVector.push_back({ ActionType::EXPR, i });
		};

	opFormAndStringMap["Integer.add"] = opFormAndStringMap["Real.add"] = { OpForm::BINARY, " + " };
	opFormAndStringMap["Integer.sub"] = opFormAndStringMap["Real.sub"] = { OpForm::BINARY, " - " };
	opFormAndStringMap["Integer.mul"] = opFormAndStringMap["Real.mul"] = { OpForm::BINARY, " * " };
	opFormAndStringMap["Integer.div"] = opFormAndStringMap["Real.div"] = { OpForm::BINARY, " / " };
	opFormAndStringMap["Integer.neg"] = opFormAndStringMap["Real.neg"] = { OpForm::LEFT_UNARY, "-" };
	opFormAndStringMap["Integer.mod"] = { OpForm::BINARY, " % " };
	opFormAndStringMap["Integer.eq"] = opFormAndStringMap["Real.eq"] = { OpForm::BINARY, " == " };
	opFormAndStringMap["Integer.ne"] = opFormAndStringMap["Real.ne"] = { OpForm::BINARY, " != " };
	opFormAndStringMap["Integer.lt"] = opFormAndStringMap["Real.lt"] = { OpForm::BINARY, " < " };
	opFormAndStringMap["Integer.le"] = opFormAndStringMap["Real.le"] = { OpForm::BINARY, " <= " };
	opFormAndStringMap["Integer.gt"] = opFormAndStringMap["Real.gt"] = { OpForm::BINARY, " > " };
	opFormAndStringMap["Integer.ge"] = opFormAndStringMap["Real.ge"] = { OpForm::BINARY, " >= " };
	opFormAndStringMap["Logic.and"] = { OpForm::BINARY, " && " };
	opFormAndStringMap["Logic.or"] = { OpForm::BINARY, " || " };
	opFormAndStringMap["Logic.not"] = { OpForm::LEFT_UNARY, "!" };
	opFormAndStringMap["Integer.max"] = { OpForm::FUNCTION, "max" };
	opFormAndStringMap["Integer.min"] = { OpForm::FUNCTION, "min" };
}

std::string PrintSdslProcedurePass::getExpr(uint32_t i) const
{
	auto s = PrintProcedurePass::getExpr(i);
	switch (printVertexVector[i].form)
	{
	case OpForm::NUMERAL:
	case OpForm::FUNCTION:
	case OpForm::LEFT_UNARY:
	case OpForm::RIGHT_UNARY:
	case OpForm::BINARY:
	case OpForm::ARRAY_GET:
		if (dynamic_cast<ConstantValue*>(vertexAt(i)) != nullptr)
		{
			s = "const " + s;
		}
		else if (!isOutput.at(i))
		{
			s = "var " + s;
		}
	}
	return s;
}

std::string PrintSdslProcedurePass::getNumeralExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = dynamic_cast<ConstantValue*>(vertexAt(i));
	ss << printVertexVector[i].name << " = " << getTypeString(value->getType()) << '(' << value->getValue().toString() << ')';
	return ss.str();
}

std::string PrintSdslProcedurePass::getArrayGetExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	const auto& operands = getOperandsByVertex(i);
	ss << printVertexVector[i].name << " = " << printVertexVector[operands[0]].name << '[';
	for (uint32_t j = 1; j < operands.size(); j++)
	{
		ss << printVertexVector[operands[j]].name;
		if (j != operands.size() - 1)
		{
			ss << ", ";
		}
	}
	ss << ']';
	return ss.str();
}

std::string PrintSdslProcedurePass::getArraySetExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	const auto& operands = getOperandsByVertex(i);
	ss << printVertexVector[i].name << '[';
	for (uint32_t j = 1; j < operands.size() - 1; j++)
	{
		ss << printVertexVector[operands[j]].name;
		if (j < operands.size() - 2)
		{
			ss << ", ";
		}
	}
	ss << "] = " << printVertexVector[operands.back()].name;
	return ss.str();
}

void PrintSdslProcedurePass::printFunctionHead(Section* section, uint32_t i)
{
	uint32_t lower = uint32_t(std::lower_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	uint32_t upper = uint32_t(std::upper_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	std::vector<Value*> results;
	for (uint32_t j = lower; j < upper; j++)
	{
		if (isOutput.at(j))
		{
			results.push_back(vertexAt(j));
		}
	}
	out << "func ";
	for (uint32_t j = 0; j < results.size(); j++)
	{
		out << getTypeString(results[j]->getType()) << ' ' << printVertexVector[vertexId(results[j])].name;
		if (j < results.size() - 1)
		{
			out << ", ";
		}
	}
	out << " = " << getSectionNameString(section) << '(';
	auto params(section->getParameterVector());
	if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(section))
	{
		params.insert(params.end(), sectionGenerator->getTemplateVector().begin(), sectionGenerator->getTemplateVector().end());
	}
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

void PrintSdslProcedurePass::printFunctionTail(Section* section, uint32_t i)
{
	printTabs(--tabNum);
	out << "end func\n\n";
}

void PrintSdslProcedurePass::printFunctionCall(SectionCall* call, uint32_t i)
{
	auto callee = call->getCallee();
	const auto& results = call->getResultValueVector();
	if (results.empty())
	{
		return;
	}
	for (uint32_t j = 0; j < results.size(); j++)
	{
		if (!isOutput.at(vertexId(results[j])))
		{
			printTabs(tabNum);
			out << "var " << printVertexVector[vertexId(results[j])].name << " = " << getTypeString(results[j]->getType()) << "()\n";
		}
	}
	PrintProcedurePass::printFunctionCall(call, i);
}

void PrintSdslProcedurePass::printBinaryBranchBegin(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum++);
	out << "if " << getValueNameString(branch->getCondition()) << '\n';
}

void PrintSdslProcedurePass::printBinaryBranchElse(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum - 1);
	out << "else\n";
}

void PrintSdslProcedurePass::printBinaryBranchEnd(BinaryBranch* branch, uint32_t i)
{
	printTabs(--tabNum);
	out << "end if\n";
}

void PrintSdslProcedurePass::printIterateLoopBegin(IterateLoop* loop, uint32_t i)
{
	printTabs(tabNum++);
	out << "do " << getValueNameString(loop->getCounter()) << " = ";
	auto str_begin = getValueNameString(loop->getBegin());
	auto str_times = getValueNameString(loop->getTimes());
	auto str_step = getValueNameString(loop->getStep());
	out << str_begin << ", " << str_begin << " + (" << str_times << " - 1) * " << str_step << ", " << str_step << '\n';
}

void PrintSdslProcedurePass::printIterateLoopEnd(IterateLoop* loop, uint32_t i)
{
	printTabs(--tabNum);
	out << "end do\n";
}

void PrintSdslProcedurePass::printParallelLoopBegin(ParallelLoop* loop, uint32_t i)
{
	printTabs(tabNum++);
	for (size_t n = 0; n < loop->getCounterNum(); n++)
	{
		out << (n == 0 ? "for " : "; ") << getValueNameString(loop->getCounter(n)) << " = ";
		auto str_array = getValueNameString(loop->getPhiValue(0));
		out << "lbound(" << str_array << "), ubound(" << str_array << ")\n";
	}
	out << '\n';
}

void PrintSdslProcedurePass::printParallelLoopEnd(ParallelLoop* loop, uint32_t i)
{
	printTabs(--tabNum);
	out << "end for\n";
}

void PrintSdslModulePass::runOnSubgraph(uint32_t i)
{
	PrintSdslProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}
