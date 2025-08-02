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

/// 
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

/// 
class CompileUnitParser final : public Parser
{
public:
	explicit CompileUnitParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	SDSL_API std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

#ifdef PARSER_CPP
/// 
class FunctionDefinitionParser final : public Parser
{
public:
	explicit FunctionDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class VariableDefinitionParser final : public Parser
{
public:
	explicit VariableDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class StructDefinitionParser final : public Parser
{
public:
	explicit StructDefinitionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class StatementParser final : public Parser
{
public:
	explicit StatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// if
class IfStatementParser final : public Parser
{
public:
	explicit IfStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// Do
class DoStatementParser final : public Parser
{
public:
	explicit DoStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// For
class ForStatementParser final : public Parser
{
public:
	explicit ForStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class AssignStatementParser final : public Parser
{
public:
	explicit AssignStatementParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/**
* @brief 
*/
class LeftValueExpressionParser final : public Parser
{
public:
	explicit LeftValueExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class RightValueExpressionParser final : public Parser
{
public:
	explicit RightValueExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class ArraySubscriptExpressionParser final : public Parser
{
public:
	explicit ArraySubscriptExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};

/// 
class ArgumentListExpressionParser final : public Parser
{
public:
	explicit ArgumentListExpressionParser(ErrorHandler& errorHandler) : Parser(errorHandler) {}

	std::unique_ptr<AST> run(std::queue<Token>& tokenStream);
};
#endif
