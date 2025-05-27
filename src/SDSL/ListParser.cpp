#define PARSER_CPP
#include "Parser.h"

std::unique_ptr<AST> ArraySubscriptExpressionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();

LBRACKET: {
	switch (readToken().category)
	{
	case Token::Category::QUESTION:
		astLeafCreate();
		nextToken();
		goto RBRACKET;
	default:
		RECURSE(RightValueExpressionParser, "在分析数组下标时发现语法错误");
		goto RBRACKET;
	}
}

RBRACKET: {
	switch (readToken().category)
	{
	case Token::Category::COLON:
		astLeafCreate();
		nextToken();
		goto LBRACKET;
	case Token::Category::COMMA:
		astLeafCreate();
		nextToken();
		goto LBRACKET;
	case Token::Category::RBRACKET:
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::LBRACKET)
		{
			astLeafCreate();
			nextToken();
			goto LBRACKET;
		}
		else
		{
			goto SUCCESS;
		}
	default:
		ERROR("RBRACKET")
	}
}

SUCCESS:
	astNodeReduce(AST::Category::ARRAY_SUBSCRIPT);
	return std::move(astStack.back());
}

std::unique_ptr<AST> ArgumentListExpressionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();

CONTENT: {
	switch (readToken().category)
	{
	case Token::Category::RPAREN: // 未初始化
		astLeafCreate();
		nextToken();
		goto SUCCESS_FUNC;
	default:
		RECURSE(RightValueExpressionParser, "在分析参数列表表达式时发现语法错误");
		goto COMMA;
	}
}

COMMA: {
	switch (readToken().category)
	{
	case Token::Category::COMMA:
		astLeafCreate();
		nextToken();
		goto CONTENT;
	case Token::Category::RPAREN:
		astLeafCreate();
		nextToken();
		goto SUCCESS_FUNC;
	case Token::Category::RBRACE:
		astLeafCreate();
		nextToken();
		goto SUCCESS_TUPLE;
	default:
		ERROR("COMMA")
	}
}

SUCCESS_FUNC:
	astNodeReduce(AST::Category::ARGUMENT_LIST);
	return std::move(astStack.back());

SUCCESS_TUPLE:
	astNodeReduce(AST::Category::TUPLE_INITIALIZER);
	return std::move(astStack.back());
}
