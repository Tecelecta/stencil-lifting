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
	// ERROR("在分析函数定义时，未识别到返回值")
	ERROR("Return value not found when parsing function")
}

RETURN_ID: {
	switch (readToken().category)
	{
	case Token::Category::LBRACKET:
		// "在数组表达式中发现语法错误"
		RECURSE(ArraySubscriptExpressionParser, "Syntax error when parsing Array Expr");
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
		// ERROR("在分析函数定义时，未识别到赋值号'='")
		ERROR("Assgining operator '=' not found when parsing function defs")
	default: {
		auto message = readToken().isKeyword() ? 
			"Keywords as return variable name when parsing function defs" : //"在分析函数定义时，使用关键字作为返回值名" 
			"Return variable name not found when parsing function defs"; //"在分析函数定义时，未识别到返回值名"
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
		// ERROR("在分析函数定义时，未识别到函数参数")
		ERROR("Parameter not found when parsing function defs")
	}
	auto message = readToken().isKeyword() ? "在分析函数定义时，使用关键字作为函数名" : "在分析函数定义时，未识别到函数名";
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
		ERROR("在分析函数定义时，识别到未定义类型")
	}
}

PARA_ID: {
	switch (readToken().category)
	{
	case Token::Category::LBRACKET:
		RECURSE(ArraySubscriptExpressionParser, "在数组表达式中发现语法错误");
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
		ERROR("在分析函数定义时，参数名后应为逗号或右括号")
	default: {
		auto message = readToken().isKeyword() ? "在分析函数定义时，使用关键字作为参数名" : "在分析函数定义时，未识别到参数名";
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
		ERROR("在分析函数定义时，未正确结束函数体")
	case Token::Category::FUNC:
		stmtCnt++;
		RECURSE(FunctionDefinitionParser, "在分析嵌套函数定义时,发现语法错误");
		goto FUNC_BODY;
	default:
		stmtCnt++;
		RECURSE(StatementParser, "在分析函数定义时，发现函数体语法错误");
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
	ERROR("在分析变量定义时，变量名语法错误")
}

ASG: {
	if (readToken().category == Token::Category::ASG)
	{
		astLeafCreate();
		nextToken();
		RECURSE(RightValueExpressionParser, "在分析变量初始化时，发现语法错误");
		goto SUCCESS;
	}
	else if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		goto VAR_NAME;
	}
	ERROR("在分析变量定义时，未发现赋值符号'='")
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
	auto message = readToken().isKeyword() ? "在分析结构体定义时，使用关键字作为结构体名" : "在分析结构体定义时，未识别到结构体名";
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
		ERROR("在分析结构体定义时，结构体没有成员")
	default:
		ERROR("在分析结构体定义时，识别到未定义的成员类型")
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
		ERROR("在分析结构体定义时，未正确结束结构体定义")
	case Token::Category::VAR:
	case Token::Category::CONST:
		stmtCnt++;
		RECURSE(VariableDefinitionParser, "在分析结构体成员定义时发现语法错误");
		goto STRUCT_BODY;
	default:
		ERROR("在分析结构体定义时，识别到未定义的成员类型")
	}
}

CSTYLE_TYPE: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto CSTYLE_TYPE;
	case Token::Category::ASG:
		ERROR("TODO：在结构体内对成员赋初值")
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
		ERROR("在分析结构体定义时，未正确结束结构体定义")
	case Token::Category::IDENTIFIER:
		stmtCnt++;
		astLeafCreate();
		nextToken();
		goto CSTYLE_NAME;
	default:
		ERROR("在分析结构体定义时，识别到未定义的成员类型")
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
		ERROR("在分析结构体定义时，识别到未定义的成员类型")
	}
}

SUCCESS:
	astNodeReduce(AST::Category::STRUCT_DEFINITION);
	return std::move(astStack.back());
}
