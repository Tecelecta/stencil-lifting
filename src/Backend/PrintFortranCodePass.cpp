#define BACKEND_EXPORTS
#include "PrintFortranCodePass.h"

PrintFortranProcedurePass::PrintFortranProcedurePass(std::ostream& out)
	: PrintProcedurePass(out)
{
	visitConstantValue = [this](ConstantValue* value, uint32_t i)
		{
			printVertexVector[i].form = OpForm::NUMERAL;
			printConstantValue.push_back(i);
		};

	opFormAndStringMap["Integer.add"] = opFormAndStringMap["Real.add"] = { OpForm::BINARY, " + " };
	opFormAndStringMap["Integer.sub"] = opFormAndStringMap["Real.sub"] = { OpForm::BINARY, " - " };
	opFormAndStringMap["Integer.mul"] = opFormAndStringMap["Real.mul"] = { OpForm::BINARY, " * " };
	opFormAndStringMap["Integer.div"] = opFormAndStringMap["Real.div"] = { OpForm::BINARY, " / " };
	opFormAndStringMap["Integer.neg"] = opFormAndStringMap["Real.neg"] = { OpForm::LEFT_UNARY, "-" };
	opFormAndStringMap["Integer.eq"] = opFormAndStringMap["Real.eq"] = { OpForm::BINARY, " == " };
	opFormAndStringMap["Integer.ne"] = opFormAndStringMap["Real.ne"] = { OpForm::BINARY, " != " };
	opFormAndStringMap["Integer.lt"] = opFormAndStringMap["Real.lt"] = { OpForm::BINARY, " < " };
	opFormAndStringMap["Integer.le"] = opFormAndStringMap["Real.le"] = { OpForm::BINARY, " <= " };
	opFormAndStringMap["Integer.gt"] = opFormAndStringMap["Real.gt"] = { OpForm::BINARY, " > " };
	opFormAndStringMap["Integer.ge"] = opFormAndStringMap["Real.ge"] = { OpForm::BINARY, " >= " };
	opFormAndStringMap["Logic.and"] = { OpForm::BINARY, " .and. " };
	opFormAndStringMap["Logic.or"] = { OpForm::BINARY, " .or. " };
	opFormAndStringMap["Logic.not"] = { OpForm::LEFT_UNARY, ".not. " };
	opFormAndStringMap["Integer.max"] = { OpForm::FUNCTION, "MAX" };
	opFormAndStringMap["Integer.min"] = { OpForm::FUNCTION, "MIN" };
}

std::string PrintFortranProcedurePass::getTypeString(Type type) const
{
	if (type.getName().equals("Logic"))
	{
		return "Logical";
	}
	if (auto arrayType = type.cast<ArrayType>(); arrayType != nullptr)
	{
		std::stringstream ss;
		ss << arrayType.getElementType().toString() << ", dimension(";
		for (size_t i = 0; i < arrayType.getNumDims(); i++)
		{
			if (arrayType.getDim(i).lower != nullptr)
			{
				ss << arrayType.getDim(i).lower.getData();
			}
			ss << ':';
			if (arrayType.getDim(i).upper != nullptr)
			{
				ss << arrayType.getDim(i).upper.getData();
			}
			if (i < arrayType.getNumDims() - 1)
			{
				ss << ',';
			}
		}
		ss << ')';
		return ss.str();
	}
	return type.toString();
}

std::string PrintFortranProcedurePass::getValueNameString(Value* value) const
{
	value = parent->vertexAt(parent->arrayBaseVector[parent->vertexId(value)]);
	return PrintProcedurePass::getValueNameString(value);
}

std::string PrintFortranProcedurePass::getArrayGetExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	const auto& operands = getOperandsByVertex(i);
	ss << printVertexVector[i].name << " = " << printVertexVector[operands[0]].name << '(';
	for (uint32_t j = 1; j < operands.size(); j++)
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

std::string PrintFortranProcedurePass::getArraySetExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = vertexAt(i);
	const auto& operands = getOperandsByVertex(i);
	ss << printVertexVector[i].name << '(';
	for (uint32_t j = 1; j < operands.size() - 1; j++)
	{
		ss << printVertexVector[operands[j]].name;
		if (j < operands.size() - 2)
		{
			ss << ", ";
		}
	}
	ss << ") = " << printVertexVector[operands.back()].name;
	return ss.str();
}

void PrintFortranProcedurePass::printFunctionHead(Section* section, uint32_t i)
{
	out << "subroutine " << getSectionNameString(section) << "(\n";
	printTabs(tabNum + 2);
	std::vector<Value*> params(section->getParameterVector().begin(), section->getParameterVector().end());
	if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(section))
	{
		params.insert(params.end(), sectionGenerator->getTemplateVector().begin(), sectionGenerator->getTemplateVector().end());
	}
	uint32_t lower = uint32_t(std::lower_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	uint32_t upper = uint32_t(std::upper_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	for (uint32_t j = lower; j < upper; j++)
	{
		if (isOutput.at(j))
		{
			auto k = parent->vertexId(vertexAt(j));
			if (parent->arrayBaseVector[k] != k)
			{
				isOutput[vertexId(parent->vertexAt(parent->arrayBaseVector[k]))] = true;
				continue;
			}
			params.push_back(vertexAt(j));
		}
	}
	for (uint32_t j = 0; j < params.size(); j++)
	{
		out << printVertexVector[vertexId(params[j])].name;
		if (j < params.size() - 1)
		{
			out << ", &\n";
			printTabs(tabNum + 2);
		}
	}
	out << ")\n";
	tabNum += 1;
	printLocalValues(section, i);
}

void PrintFortranProcedurePass::printFunctionTail(Section* section, uint32_t i)
{
	printTabs(--tabNum);
	out << "end subroutine\n\n";
}

void PrintFortranProcedurePass::printFunctionCall(SectionCall* call, uint32_t i)
{
	printTabs(tabNum);
	out << "call ";
	auto callee = call->getCallee();
	out << getSectionNameString(callee) << '(';
	auto args(call->getArgumentVector());
	args.insert(args.end(), call->getSpecializerVector().begin(), call->getSpecializerVector().end());
	for (uint32_t j = 0; j < args.size(); j++)
	{
		out << printVertexVector[vertexId(args[j])].name;
		if (j < args.size() - 1)
		{
			out << ", &\n";
			printTabs(tabNum + 2);
		}
	}
	out << ")\n";
}

void PrintFortranProcedurePass::printBinaryBranchBegin(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum++);
	out << "if (" << getValueNameString(branch->getCondition()) << ") then\n";
}

void PrintFortranProcedurePass::printBinaryBranchElse(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum - 1);
	out << "else\n";
}

void PrintFortranProcedurePass::printBinaryBranchEnd(BinaryBranch* branch, uint32_t i)
{
	printTabs(--tabNum);
	out << "end if\n";
}

void PrintFortranProcedurePass::printIterateLoopBegin(IterateLoop* loop, uint32_t i)
{
	printTabs(tabNum++);
	out << "do " << getValueNameString(loop->getCounter()) << " = ";
	auto str_begin = getValueNameString(loop->getBegin());
	auto str_times = getValueNameString(loop->getTimes());
	auto str_step = getValueNameString(loop->getStep());
	out << str_begin << ", " << str_begin << " + (" << str_times << " - 1) * " << str_step << ", " << str_step << '\n';
}

void PrintFortranProcedurePass::printIterateLoopEnd(IterateLoop* loop, uint32_t i)
{
	printTabs(--tabNum);
	out << "end do\n";
}

void PrintFortranProcedurePass::printParallelLoopBegin(ParallelLoop* loop, uint32_t i)
{
	for (size_t n = 0; n < loop->getCounterNum(); n++)
	{
		printTabs(tabNum++);
		out << "do "<< getValueNameString(loop->getCounter(n)) << " = ";
		auto str_array = getValueNameString(loop->getPhiValue(0));
		out << "LBOUND(" << str_array << "), UBOUND(" << str_array << ")\n";
	}
}

void PrintFortranProcedurePass::printParallelLoopEnd(ParallelLoop* loop, uint32_t i)
{
	for (size_t n = 0; n < loop->getCounterNum(); n++)
	{
		printTabs(--tabNum);
		out << "end do\n";
	}
}

void PrintFortranProcedurePass::printLocalValues(Section* section, uint32_t i)
{
	uint32_t lower = uint32_t(std::lower_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	uint32_t upper = uint32_t(std::upper_bound(vertexSubgraphVector.begin(), vertexSubgraphVector.end(), i) - vertexSubgraphVector.begin());
	for (auto j : printConstantValue)
	{
		auto value = dynamic_cast<ConstantValue*>(vertexAt(j));
		printTabs(tabNum);
		out << getTypeString(vertexAt(j)->getType()) << ", parameter";
		out << " :: " << getExpr(j) << '\n';
	}
	for (uint32_t j = lower; j < upper; j++)
	{
		auto value = vertexAt(j);
		auto k = parent->vertexId(value);
		if (parent->arrayBaseVector[k] != k)
		{
			continue;
		}
		printTabs(tabNum);
		out << getTypeString(vertexAt(j)->getType());
		if (isOutput.at(j))
		{
			if (isInput.at(j))
			{
				out << ", intent(inout)";
			}
			else
			{
				out << ", intent(out)";
			}
		}
		else if(isInput.at(j))
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				out << ", intent(in)";
			}
			else
			{
				out << ", value";
			}
		}
		out << " :: " << getValueNameString(value) << '\n';
	}
}

void PrintFortranModulePass::run(GraphValueProjection src, const std::vector<Value*>& returnValues, std::string_view moduleName)
{
	out << "module " << moduleName << "\n!DIR$ FREEFORM\ncontains\n\n";
	PrintModulePass::run(src, returnValues);
	out << "end module " << moduleName << "\n\n";
}

void PrintFortranModulePass::runOnSubgraph(uint32_t i)
{
	PrintFortranProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}
