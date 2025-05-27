#define PARSER_CPP
#include "Parser.h"

std::unique_ptr<AST> StatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	switch (readToken().category)
	{
	case Token::Category::DO:
		RECURSE(DoStatementParser, "在分析Do语句时发现语法错误");
		break;
	case Token::Category::FOR:
		RECURSE(ForStatementParser, "在分析for语句时发现语法错误");
		break;
	case Token::Category::IF:
		RECURSE(IfStatementParser, "在分析if语句时发现语法错误");
		break;
	case Token::Category::CONST:
		RECURSE(VariableDefinitionParser, "在分析局部常量定义时发现语法错误");
		break;
	case Token::Category::VAR:
		RECURSE(VariableDefinitionParser, "在分析局部变量定义时发现语法错误");
		break;
	case Token::Category::END_OF_LINE:
	case Token::Category::SEMICOLON:
		astLeafCreate();
		nextToken();
		break;
	default:
		RECURSE(AssignStatementParser, "在分析赋值语句时发现语法错误");
		switch (readToken().category)
		{
		case Token::Category::RPAREN:
			ERROR("表达式后发现多余的')'");
		case Token::Category::RBRACKET:
			ERROR("表达式后发现多余的']'");
		case Token::Category::RBRACE:
			ERROR("表达式后发现多余的'}'");
		}
		break;
	}
	return std::move(astStack.back());
}

std::unique_ptr<AST> AssignStatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	goto LEFT;
LEFT: {
	RECURSE(LeftValueExpressionParser, "在分析左值表达式时，发现语法错误");
	goto ASG;
}

ASG: {
	switch (readToken().category)
	{
	case Token::Category::ASG: case Token::Category::ADD_ASG: case Token::Category::SUB_ASG: case Token::Category::MUL_ASG: case Token::Category::DIV_ASG:	case Token::Category::MOD_ASG:	case Token::Category::BIT_AND_ASG:	case Token::Category::BIT_OR_ASG: case Token::Category::BIT_XOR_ASG: case Token::Category::SHL_ASG:	case Token::Category::ASHR_ASG:
		astLeafCreate();
		nextToken();
		goto RIGHT;
	case Token::Category::COMMA: // 支持多变量赋值
		astLeafCreate();
		nextToken();
		goto LEFT;
	default:
		ERROR("在分析赋值语句时，发现缺少赋值符号")
	}
}

RIGHT: {
	RECURSE(RightValueExpressionParser, "在分析右值表达式时，发现语法错误");
	astNodeReduce(AST::Category::ASSIGN_STATEMENT);
	return std::move(astStack.back());
}
}

std::unique_ptr<AST> IfStatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();
	size_t if_body = 0;
	size_t else_body = 0;

	RECURSE(RightValueExpressionParser, "在分析IF条件表达式时发现语法错误");

IF_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto IF_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::IF_BODY, if_body);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IF)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("在分析函数定义时，未正确结束if语句")
	case Token::Category::ELSE:
		astNodeReduce(AST::Category::IF_BODY, if_body);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IF)
		{
			RECURSE(IfStatementParser, "在分析if语句时发现语法错误");
			goto SUCCESS; // 可以少一个end if
		}
		goto ELSE_BODY;
	default:
		if_body++;
		RECURSE(StatementParser, "在分析if语句时发现语法错误");
		goto IF_BODY;
	}
}

ELSE_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto ELSE_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::ELSE_BODY, else_body);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IF)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("在分析函数定义时，未正确结束else语句")
	default:
		else_body++;
		RECURSE(StatementParser, "在分析else语句时发现语法错误");
		goto ELSE_BODY;
	}
}

SUCCESS:
	astNodeReduce(AST::Category::IF_STATEMENT);
	return std::move(astStack.back());
}

std::unique_ptr<AST> DoStatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();
	size_t doBody = 0;
	goto DO_VARIABLE;

DO_VARIABLE: {
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::ASG)
		{
			astNodeReduce(AST::Category::LOOP_VARIABLE, 1);
			astLeafCreate();
			nextToken();
			goto DO_HEAD;
		}
		ERROR("在分析循环定义时，未识别到赋值")
	}
	ERROR("在分析循环定义时，未识别到循环控制变量")
}

DO_HEAD: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环控制变量开始条件");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_HEAD, 2);
		goto DO_TAIL;
	}
	ERROR("在分析循环条件时，未正确识别循环控制变量结束条件")
}

DO_TAIL: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环控制变量结束条件");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_TAIL, 2);
		goto DO_STEP;
	}
	else if (readToken().category == Token::Category::END_OF_LINE)
	{
		astNodeReduce(AST::Category::LOOP_TAIL, 1);
		goto DO_BODY;
	}
	ERROR("在分析循环条件时，未正确识别循环步长")
}

DO_STEP: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环步长");
	astNodeReduce(AST::Category::LOOP_STEP, 1);
	if (readToken().category == Token::Category::END_OF_LINE)
	{
		goto DO_BODY;
	}
	ERROR("在分析循环条件时，未正确识别循环体")
}

DO_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto DO_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::LOOP_BODY, doBody);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::DO)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("在分析函数定义时，未正确结束循环语句")
	default:
		doBody++;
		RECURSE(StatementParser, "在分析语句时发现语法错误");
		goto DO_BODY;
	}
}

SUCCESS:
	astNodeReduce(AST::Category::DO_STATEMENT);
	return std::move(astStack.back());
}

// for i; j = head, tail, step; head, tail, step  [ X ]
// for i = head, tail, step; j = head, tail, step [ √ ]
std::unique_ptr<AST> ForStatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	astLeafCreate();
	nextToken();
	size_t forBody = 0;
	size_t forKeyword = 1;

FOR_VARIABLE: {
	if (readToken().category == Token::Category::IDENTIFIER)
	{
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::ASG)
		{
			astNodeReduce(AST::Category::LOOP_VARIABLE, 1);
			astLeafCreate();
			nextToken();
			goto FOR_HEAD;
		}
		ERROR("在分析循环定义时，未识别到赋值")
	}
	ERROR("在分析循环定义时，未识别到循环控制变量")
}

FOR_HEAD: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环控制变量开始条件");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_HEAD, 2);
		goto FOR_TAIL;
	}
	ERROR("在分析循环条件时，未正确识别循环控制变量结束条件")
}

FOR_TAIL: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环控制变量结束条件");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_TAIL, 2);
		goto FOR_STEP;
	}
	else if (readToken().category == Token::Category::SEMICOLON)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_TAIL, 2);
		astNodeReduce(AST::Category::LOOP_INDEX, 4);
		forKeyword = 0;
		goto FOR_VARIABLE;
	}
	else if (readToken().category == Token::Category::END_OF_LINE)
	{
		astNodeReduce(AST::Category::LOOP_TAIL, 1);
		astNodeReduce(AST::Category::LOOP_INDEX, 4);
		goto FOR_BODY;
	}
	ERROR("在分析循环条件时，未正确识别循环步长")
}

FOR_STEP: {
	RECURSE(RightValueExpressionParser, "在分析循环条件时，未识别到循环步长");
	if (readToken().category == Token::Category::SEMICOLON)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_STEP, 2);
		astNodeReduce(AST::Category::LOOP_INDEX, 5);
		forKeyword = 0;
		goto FOR_VARIABLE;
	}
	else if (readToken().category == Token::Category::END_OF_LINE)
	{
		astNodeReduce(AST::Category::LOOP_STEP, 1);
		astNodeReduce(AST::Category::LOOP_INDEX, 5);
		goto FOR_BODY;
	}
	ERROR("在分析循环条件时，未正确识别循环体")
}

FOR_BODY: {
	switch (readToken().category)
	{
	case Token::Category::END_OF_LINE:
		nextToken();
		goto FOR_BODY;
	case Token::Category::END:
		astNodeReduce(AST::Category::LOOP_BODY, forBody);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::FOR)
		{
			astLeafCreate();
			nextToken();
			goto SUCCESS;
		}
		ERROR("在分析函数定义时，未正确结束循环语句")
	default:
		forBody++;
		RECURSE(StatementParser, "在分析语句时发现语法错误");
		goto FOR_BODY;
	}
}

SUCCESS:
	astNodeReduce(AST::Category::FOR_STATEMENT);
	return std::move(astStack.back());
}
