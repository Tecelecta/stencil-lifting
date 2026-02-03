#define PARSER_CPP
#include "Parser.h"
#include<iostream>

std::unique_ptr<AST> FunctionDefinitionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();
	size_t ptCnt = 0;
	size_t rtCnt = 0;
	size_t stmtCnt = 0; 

RETURN_TYPE: {
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		rtCnt++;
		astLeafCreate();
		nextToken();
		goto RETURN_ID;
	}
	ERROR("In function defination: return value not found")
}

RETURN_ID: {
	switch (readToken().category)
	{
	case Token::Category::LBRACKET:
		RECURSE(ArraySubscriptExpressionParser, "Syntax error in array expression");
		astNodeReduce(AST::Category::ARRAY_ELEMENT, 2);
		goto RETURN_ID;
	case Token::Category::IDENTIFIER:
		rtCnt++;
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::COMMA)
		{
			rtCnt++;
			astLeafCreate();
			nextToken();
			goto RETURN_TYPE;
		}
		else if (readToken().category == Token::Category::ASG)
		{
			astNodeReduce(AST::Category::FUNCTION_RETURN_VALUE, rtCnt);
			astLeafCreate();
			nextToken();
			goto FUNC_NAME;
		}
		ERROR("In function defination: assgin operator '=' not found")
	default: {
		auto message = readToken().isKeyword() ? 
			"Keywords as return variable name when parsing function defs" : //"In function defination: using keywords as return value name" 
			"Return variable name not found when parsing function defs"; //"In function defination: return value not found名"
		ERROR(message);
	}
	}
}

FUNC_NAME: {
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::FUNCTION_NAME, 1);
		if (readToken().category == Token::Category::LPAREN) // (
		{
			ptCnt++;
			astLeafCreate();
			nextToken();
			goto PARA_TYPE;
		}
		ERROR("In function defination: parameter not found")
	}
	auto message = readToken().isKeyword() ? "In function defination: using keywords as function name" : "In function defination: function name not found";
	ERROR(message);
}

PARA_TYPE: {
	switch (readToken().category)
	{
	case Token::Category::RPAREN:
		ptCnt++;
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::FUNCTION_PARAMETER, ptCnt);
		goto FUNC_BODY;
	case Token::Category::IDENTIFIER:
		ptCnt++;
		astLeafCreate();
		nextToken();
		goto PARA_ID;
	default:
		ERROR("In function defination: undefined type")
	}
}

PARA_ID: {
	switch (readToken().category)
	{
	case Token::Category::LBRACKET:
		RECURSE(ArraySubscriptExpressionParser, "Syntax error in array expression");
		astNodeReduce(AST::Category::ARRAY_ELEMENT, 2);
		goto PARA_ID;
	case Token::Category::IDENTIFIER:
		ptCnt++;
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::COMMA)
		{
			ptCnt++;
			astLeafCreate();
			nextToken();
			goto PARA_TYPE;
		}
		else if (readToken().category == Token::Category::RPAREN)
		{
			ptCnt++;
			astLeafCreate();
			nextToken();
			astNodeReduce(AST::Category::FUNCTION_PARAMETER, ptCnt);
			goto FUNC_BODY;
		}
		ERROR("In function defination: the following token after parameter name should be ',' or ')'")
	default: {
		auto message = readToken().isKeyword() ? "In function defination: keywords as parameter name" : "In function defination: parameter name not found";
		ERROR(message);
	}
	}
}

FUNC_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto FUNC_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::FUNCTION_BODY, stmtCnt);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::FUNC)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("In function defination: incorrect ending of function body")
	case Token::Category::FUNC:
		stmtCnt++;
		RECURSE(FunctionDefinitionParser, "In nested function defination: syntax error");
		goto FUNC_BODY;
	default:
		stmtCnt++;
		RECURSE(StatementParser, "In function defination: synatx error in function body");
		goto FUNC_BODY;
	}
}

SUCCESS:
	astNodeReduce(AST::Category::FUNCTION_DEFINITION);
	return std::move(astStack.back());
}

std::unique_ptr<AST> VariableDefinitionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	auto astCategory = readToken().category == Token::Category::CONST ?
		AST::Category::CONST_DEFINITION : AST::Category::VARIABLE_DEFINITION;
	astLeafCreate();
	nextToken();
	goto VAR_NAME;

VAR_NAME: { 
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		astLeafCreate();
		nextToken();
		goto ASG;
	}
	ERROR("In function defination: synatx error in variable name")
}

ASG: {
	if (readToken().category == Token::Category::ASG)
	{
		astLeafCreate();
		nextToken();
		RECURSE(RightValueExpressionParser, "In variable defination: syntax error");
		goto SUCCESS;
	}
	else if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		goto VAR_NAME;
	}
	ERROR("In variable defination: assign operator '=' not found")
}

SUCCESS:
	astNodeReduce(astCategory);
	return std::move(astStack.back());
}

std::unique_ptr<AST> StructDefinitionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();
	size_t stmtCnt = 0;
	goto STRUCT_NAME;

STRUCT_NAME: {
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		astLeafCreate();
		nextToken();
		goto SELECT;
	}
	auto message = readToken().isKeyword() ? "In structure defination: using keywords as structure name" : "In structure defination: structure name not found";
	ERROR(message);
}

SELECT: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto SELECT;
	case Token::Category::VAR:
	case Token::Category::CONST:
		goto STRUCT_BODY;
	case Token::Category::IDENTIFIER:
		goto CSTYLE_TYPE;
	case Token::Category::END:
		ERROR("In structure defination: member declaration not found")
	default:
		ERROR("In structure defination: member type undefined")
	}
}
STRUCT_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto STRUCT_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::STRUCT_BODY, stmtCnt);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::STRUCT)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("In structure defination: incorrect structure ending")
	case Token::Category::VAR:
	case Token::Category::CONST:
		stmtCnt++;
		RECURSE(VariableDefinitionParser, "In structure defination: syntax error in member defination");
		goto STRUCT_BODY;
	default:
		ERROR("In structure defination: member type undefined")
	}
}

CSTYLE_TYPE: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto CSTYLE_TYPE;
	case Token::Category::ASG:
		ERROR("TODO: initialize member value in structure defination")
	case Token::Category::END:
		astNodeReduce(AST::Category::STRUCT_BODY, stmtCnt);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::STRUCT)
		{
			astLeafCreate();
			nextToken();
			nextToken();
			goto SUCCESS;
		}
		ERROR("In structure defination: incorrect structure ending")
	case Token::Category::IDENTIFIER:
		stmtCnt++;
		astLeafCreate();
		nextToken();
		goto CSTYLE_NAME;
	default:
		ERROR("In structure defination: member type undefined")
	}
}

CSTYLE_NAME: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto CSTYLE_NAME;
	case Token::Category::IDENTIFIER:
		astLeafCreate();
		nextToken();
		goto CSTYLE_TYPE;
	default:
		ERROR("In structure defination: member type undefined")
	}
}

SUCCESS:
	astNodeReduce(AST::Category::STRUCT_DEFINITION);
	return std::move(astStack.back());
}
