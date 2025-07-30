#define BACKEND_EXPORTS
#include "PrintHalideCodePass.h"

PrintHalideProcedurePass::PrintHalideProcedurePass(std::ostream& out)
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
	opFormAndStringMap["Integer.max"] = opFormAndStringMap["Real.max"] = { OpForm::FUNCTION, "Halide::max" };
	opFormAndStringMap["Integer.min"] = opFormAndStringMap["Real.min"] = { OpForm::FUNCTION, "Halide::min" };
	opFormAndStringMap["Integer.abs"] = opFormAndStringMap["Real.abs"] = { OpForm::FUNCTION, "Halide::abs" };
	opFormAndStringMap["Real.sqrt"] = { OpForm::FUNCTION, "Halide::sqrt" };
	opFormAndStringMap["Real.exp"] = { OpForm::FUNCTION, "Halide::exp" };
	opFormAndStringMap["Real.sin"] = { OpForm::FUNCTION, "Halide::sin" };
	opFormAndStringMap["Real.cos"] = { OpForm::FUNCTION, "Halide::cos" };
	opFormAndStringMap["Real.tan"] = { OpForm::FUNCTION, "Halide::tan" };
	opFormAndStringMap["Real.asin"] = { OpForm::FUNCTION, "Halide::asin" };
	opFormAndStringMap["Real.acos"] = { OpForm::FUNCTION, "Halide::acos" };
	opFormAndStringMap["Real.atan"] = { OpForm::FUNCTION, "Halide::atan" };
	opFormAndStringMap["Real.log"] = { OpForm::FUNCTION, "Halide::log" };
	opFormAndStringMap["Real.pow"] = { OpForm::FUNCTION, "Halide::pow" };
	opFormAndStringMap["select"] = { OpForm::FUNCTION, "Halide::select" };
}

std::string PrintHalideProcedurePass::getTypeString(Type type) const
{
	return type.cast<ArrayType>() != nullptr ? "Halide::Func" : "Halide::Expr";
}

std::string PrintHalideProcedurePass::getExpr(uint32_t i) const
{
	auto value = vertexAt(i);
	if (auto phiValue = dynamic_cast<PhiValue*>(value))
	{
		if (auto branch = dynamic_cast<BinaryBranch*>(phiValue->getSection()))
		{
			auto n = phiValue->getType().getNumDims();
			if (n == 0)
			{
				std::stringstream ss;
				if (!isOutput[i])
				{
					ss << "auto ";
				}
				ss << printVertexVector[i].name << " = Halide::select(";
				ss << printVertexVector[vertexId(branch->getCondition())].name << ", ";
				ss << printVertexVector[vertexId(phiValue->getIncoming(0))].name << ", ";
				ss << printVertexVector[vertexId(phiValue->getIncoming(1))].name << ");";
				return ss.str();
			}
			else
			{
				std::stringstream ss;
				auto printCounters = [&ss, n]()
					{
						ss << '(';
						for (size_t j = 0; j < n; j++)
						{
							ss << "Halide::Var(\"v" << j << (j == n - 1 ? "\"))" : "\"), ");
						}
					};
				ss << printVertexVector[i].name;
				printCounters();
				ss << " = Halide::select(";
				ss << printVertexVector[vertexId(branch->getCondition())].name;
				ss << ", " << printVertexVector[vertexId(phiValue->getIncoming(0))].name;
				printCounters();
				ss << ", " << printVertexVector[vertexId(phiValue->getIncoming(1))].name;
				printCounters();
				ss << ");";
				return ss.str();
			}
		}
		else if (auto loop = dynamic_cast<ParallelLoop*>(phiValue->getSection()))
		{
			std::stringstream ss;
			ss << printVertexVector[i].name << " = ";
			ss << printVertexVector[vertexId(phiValue->getIncoming(1))].name << ';';
			return ss.str();
		}
	}
	else
	{
		std::stringstream ss;
		switch (printVertexVector[i].form)
		{
		case OpForm::NUMERAL:
		case OpForm::FUNCTION:
		case OpForm::LEFT_UNARY:
		case OpForm::RIGHT_UNARY:
		case OpForm::BINARY:
		case OpForm::ARRAY_GET:
		case OpForm::PHI:
			if (auto boundValue = dynamic_cast<BoundValue*>(value))
			{
				ss << "Halide::Var " << printVertexVector[i].name << "(\"v" << boundValue->getIndex() << "\");";
			}
			else
			{
				if (i >= isOutput.size() || !isOutput.at(i))
				{
					ss << getTypeString(value->getType()) << ' ';
				}
				ss << PrintProcedurePass::getExpr(i) << ';';
			}
			return ss.str();
		default:
			return PrintProcedurePass::getExpr(i) + ';';
		}
	}
	return std::string();
}

std::string PrintHalideProcedurePass::getNumeralExpr(uint32_t i) const
{
	std::stringstream ss;
	auto value = dynamic_cast<ConstantValue*>(vertexAt(i));
	if (value->getType().getName().equals("Logic"))
	{
		ss << printVertexVector[i].name << " = " << (value->getValue().cast<Logic>().isTrue()
			? "Halide::Expr(0) == Halide::Expr(0)" : "Halide::Expr(1) == Halide::Expr(0)");
	}
	else
	{
		ss << printVertexVector[i].name << " = " << getTypeString(value->getType()) << '(' << value->getValue().toString() << ')';
	}
	return ss.str();
}

std::string PrintHalideProcedurePass::getArrayGetExpr(uint32_t i) const
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

std::string PrintHalideProcedurePass::getArraySetExpr(uint32_t i) const
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

void PrintHalideProcedurePass::printFunctionHead(Section* section, uint32_t i)
{
	if (!section->getName().isValid())
	{
		out << "static ";
	}
	out << "void " << getSectionNameString(section) << "(\n";
	printTabs(++tabNum);
	std::vector<Value*> params(section->getParameterVector().begin(), section->getParameterVector().end());
	if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(section))
	{
		params.insert(params.end(), sectionGenerator->getTemplateVector().begin(), sectionGenerator->getTemplateVector().end());
	}
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
	for (uint32_t j = 0; j < params.size(); j++)
	{
		uint32_t v = vertexId(params[j]);
		out << getTypeString(params[j]->getType()) << ' ' << printVertexVector[v].name << ",\n";
		printTabs(tabNum);
	}
	for (uint32_t j = 0; j < results.size(); j++)
	{
		uint32_t v = vertexId(results[j]);
		out << getTypeString(results[j]->getType()) << "& " << printVertexVector[v].name;
		if (j < results.size() - 1)
		{
			out << ",\n";
			printTabs(tabNum);
		}
	}
	out << ")\n";
	printTabs(tabNum - 1);
	out << "{\n";
}

void PrintHalideProcedurePass::printFunctionTail(Section* section, uint32_t i)
{
	printTabs(--tabNum);
	out << "}\n\n";
}

void PrintHalideProcedurePass::printFunctionCall(SectionCall* call, uint32_t i)
{
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
			if (results[j]->getType().cast<ArrayType>() != nullptr)
			{
				out << "Halide::Func ";
			}
			else
			{
				out << "Halide::Expr ";
			}
			out << printVertexVector[vertexId(results[j])].name << ";\n";
		}
	}

	printTabs(tabNum);
	auto callee = call->getCallee();
	out << getSectionNameString(callee) << '(';
	auto args(call->getArgumentVector());
	args.insert(args.end(), call->getSpecializerVector().begin(), call->getSpecializerVector().end());
	for (uint32_t j = 0; j < args.size(); j++)
	{
		out << printVertexVector[vertexId(args[j])].name << ",\n";
		printTabs(tabNum + 2);
	}
	for (uint32_t j = 0; j < results.size(); j++)
	{
		out << printVertexVector[vertexId(results[j])].name;
		if (j < results.size() - 1)
		{
			out << ",\n";
			printTabs(tabNum + 2);
		}
	}
	out << ");\n";
}

void PrintHalideProcedurePass::printBinaryBranchBegin(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum);
	out << "if (" << getValueNameString(branch->getCondition()) << ")\n";
	printTabs(tabNum++);
	out << "{\n";
}

void PrintHalideProcedurePass::printBinaryBranchElse(BinaryBranch* branch, uint32_t i)
{
	printTabs(tabNum - 1);
	out << "}\n";
	printTabs(tabNum - 1);
	out << "else\n";
	out << "{\n";
}

void PrintHalideProcedurePass::printBinaryBranchEnd(BinaryBranch* branch, uint32_t i)
{
	printTabs(--tabNum);
	out << "}\n";
}

void PrintHalideProcedurePass::printParallelLoopBegin(ParallelLoop* loop, uint32_t i)
{
	for (size_t j = 0; j < loop->getCounterNum(); j++)
	{
		printTabs(tabNum);
		out << "Halide::Var " << getValueNameString(loop->getCounter(j)) << "(\"v" << j << "\");\n";
	}
}

void PrintHalideProcedurePass::printParallelLoopEnd(ParallelLoop* loop, uint32_t i)
{
}

bool PrintHalideProcedurePass::runOnTrivial(uint32_t i)
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
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
				sectionCallVector.push_back(branch->getIfBranch());
				if (branch->getElseBranch()->getResultValueVectorSize() > 0)
				{
					printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
					sectionCallVector.push_back(branch->getElseBranch());
				}
				for (auto phiValue : branch->getPhiValueVector())
				{
					printActionVector.push_back({ ActionType::EXPR, vertexId(phiValue)});
				}
			}
			else if (auto loop = dynamic_cast<IterateLoop*>(section))
			{
				throw std::runtime_error("Halide do not support serial loop");
			}
			else if (auto loop = dynamic_cast<ParallelLoop*>(section))
			{
				printActionVector.push_back({ ActionType::FOR, I });
				printActionVector.push_back({ ActionType::CALL, (uint32_t)sectionCallVector.size() });
				sectionCallVector.push_back(loop->getBodyCall());
				printActionVector.push_back({ ActionType::END_FOR, I });
				for (auto phiValue : loop->getPhiValueVector())
				{
					printActionVector.push_back({ ActionType::EXPR, vertexId(phiValue) });
				}
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

void PrintHalideModulePass::runOnSubgraph(uint32_t i)
{
	PrintHalideProcedurePass subPass(out);
	subPass.parent = this;
	subPass.run(GraphOuterSection({ subgraphAt(i) }));
}
