#pragma once

#include "VCG/AnalyzeArrayBasePass.h"
#include "Backend_global.h"

class PrintCodePass
{
protected:
	PrintCodePass(std::ostream& out = std::cout) : out(out) {}
	DEFAULT_COPY_MOVE(PrintCodePass)

	void printTabs(size_t num)
	{
		out << std::string(num, '\t');
	}

	void printTabsUsingSpaces(size_t num)
	{
		out << std::string(num * 4, ' ');
	}

protected:
	std::ostream& out;
};

class PrintProcedurePass : virtual public Pass<GraphOuterSection>, public PrintCodePass,
	private SccAction, protected Value::Visitor<void, uint32_t>
{
public:
	BACKEND_API PrintProcedurePass(std::ostream& out = std::cout);

	BACKEND_API void run(GraphOuterSection src);

protected:
	enum class OpForm
	{
		NONE, INVALID, NUMERAL, RESULT, PHI, 
		FUNCTION, LEFT_UNARY, RIGHT_UNARY, BINARY,
		ARRAY_GET, ARRAY_SET, COPY
	};

	struct PrintVertex
	{
		OpForm form = OpForm::NONE;
		std::string op;
		std::string type;
		std::string name;
	};

	enum class ActionType
	{
		EXPR, HEAD, TAIL, CALL, IF, ELSE, END_IF, DO, END_DO, FOR, END_FOR
	};

	struct PrintAction
	{
		ActionType type = ActionType::EXPR;
		uint32_t index = 0;
	};

	virtual std::tuple<OpForm, std::string> getVertexOpFormAndString(Operation op) const;

	virtual std::string getTypeString(Type type) const;

	virtual std::string getValueNameString(Value* value) const;

	virtual std::string getSectionNameString(Section* section) const;

	virtual std::string getExpr(uint32_t i) const;

	virtual std::string getNumeralExpr(uint32_t i) const;

	virtual std::string getCallExpr(uint32_t i) const;

	virtual std::string getLeftUnaryExpr(uint32_t i) const;

	virtual std::string getRightUnaryExpr(uint32_t i) const;

	virtual std::string getBinaryExpr(uint32_t i) const;

	virtual std::string getArrayGetExpr(uint32_t i) const;

	virtual std::string getArraySetExpr(uint32_t i) const;

	virtual void printExpr(uint32_t i);

	virtual void printFunctionHead(Section* section, uint32_t i);

	virtual void printFunctionTail(Section* section, uint32_t i);

	virtual void printFunctionCall(SectionCall* call, uint32_t i);

	virtual void printBinaryBranchBegin(BinaryBranch* branch, uint32_t i);

	virtual void printBinaryBranchElse(BinaryBranch* branch, uint32_t i) {}

	virtual void printBinaryBranchEnd(BinaryBranch* branch, uint32_t i) { tabNum -= 1; }

	virtual void printIterateLoopBegin(IterateLoop* loop, uint32_t i);

	virtual void printIterateLoopEnd(IterateLoop* loop, uint32_t i) { tabNum -= 1; }

	virtual void printParallelLoopBegin(ParallelLoop* loop, uint32_t i);

	virtual void printParallelLoopEnd(ParallelLoop* loop, uint32_t i) { tabNum -= 1; }

	bool runOnTrivial(uint32_t i) override;

	bool runOnTypical(const vertex_list& scc) override;

public:
	size_t tabNum = 0;
	class PrintModulePass* parent = nullptr;

protected:
	std::vector<PrintVertex> printVertexVector;
	std::vector<PrintAction> printActionVector;
	std::vector<SectionCall*> sectionCallVector;
	std::vector<bool> isInput;
	std::vector<bool> isOutput;
	std::unordered_map<std::string, std::tuple<PrintProcedurePass::OpForm, std::string>> opFormAndStringMap;
	uint32_t nextSubgraph = 0;
};

class PrintModulePass : public AnalyzeArrayBasePass, public PrintCodePass
{
public:
	PrintModulePass(std::ostream& out = std::cout) : PrintCodePass(out) {}

	BACKEND_API void run(GraphValueProjection src, const std::vector<Value*>& returnValues);

protected:
	virtual void runOnSubgraph(uint32_t i) = 0;

public:
	std::vector<bool> isReturn;
};
