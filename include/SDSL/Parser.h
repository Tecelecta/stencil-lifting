#pragma once

#include "AST.h"
#include "ErrorHandler.h"

#include <queue>
#include <stack>

#ifdef PARSER_CPP
#define ERROR(message) errorHandler.error(message, readToken().line, readToken().column); return nullptr;
#define WARNING(message) errorHandler.warning(message, readToken().line, readToken().column);
#define INFO(message) errorHandler.info(message, readToken().line, readToken().column);
#define RECURSE(ParserClass, message) if (auto ast = ParserClass(errorHandler).run(tokenStream)) astStack.push_back(std::move(ast)); else { INFO(message) return nullptr; }
#endif

/// 语法分析器基类
class Parser
{
protected:
	explicit Parser(ErrorHandler& errorHandler) : errorHandler(errorHandler) {}
	DISABLE_COPY_MOVE(Parser)

	const Token& readToken() const { return tokenStreamPtr->front(); }

	void setup(std::queue<Token>& tokenStream);
	void nextToken();

	void astLeafCreate();
	void astNodeReduce(AST::Category category);
	void astNodeReduce(AST::Category category, size_t num);

protected:
	ErrorHandler& errorHandler;
	std::vector<std::unique_ptr<AST>> astStack;

private:
	std::queue<Token>* tokenStreamPtr = nullptr;
};

/// 用于分析整个编译单元的语法分析器
class CompileUnitParser final : public Parser
{
public:
	explicit CompileUnitParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	SDSL_API std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

#ifdef PARSER_CPP
/// 用于分析函数定义的语法分析器
class FunctionDefinitionParser final : public Parser
{
public:
	explicit FunctionDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析局部变量定义的语法分析器
class VariableDefinitionParser final : public Parser
{
public:
	explicit VariableDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析结构体定义的语法分析器
class StructDefinitionParser final : public Parser
{
public:
	explicit StructDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析语句定义的语法分析器
class StatementParser final : public Parser
{
public:
	explicit StatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析if语句定义的语法分析器
class IfStatementParser final : public Parser
{
public:
	explicit IfStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析Do语句（串行循环）定义的语法分析器
class DoStatementParser final : public Parser
{
public:
	explicit DoStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析For语句（并行循环）定义的语法分析器
class ForStatementParser final : public Parser
{
public:
	explicit ForStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析赋值语句定义的语法分析器
class AssignStatementParser final : public Parser
{
public:
	explicit AssignStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/**
* @brief 用于分析左值表达式的语法分析器
*/
class LeftValueExpressionParser final : public Parser
{
public:
	explicit LeftValueExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析右值表达式的语法分析器
class RightValueExpressionParser final : public Parser
{
public:
	explicit RightValueExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析数组下标表达式的语法分析器
class ArraySubscriptExpressionParser final : public Parser
{
public:
	explicit ArraySubscriptExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 用于分析实参列表的语法分析器，用于函数调用和元组初始化
class ArgumentListExpressionParser final : public Parser
{
public:
	explicit ArgumentListExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};
#endif
