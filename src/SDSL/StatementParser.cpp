#define PARSER_CPP
#include "Parser.h"

std::unique_ptr<AST> StatementParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	switch (readToken().category)
	{
	case Token::Category::DO:
		RECURSE(DoStatementParser, "In DO statement: syntax error");
		break;
	case Token::Category::FOR:
		RECURSE(ForStatementParser, "In FOR statement: syntax error");
		break;
	case Token::Category::IF:
		RECURSE(IfStatementParser, "In IF statement: syntax error");
		break;
	case Token::Category::CONST:
		RECURSE(VariableDefinitionParser, "In local constant defination: syntax error");
		break;
	case Token::Category::VAR:
		RECURSE(VariableDefinitionParser, "In local variable defination: syntax error");
		break;
	case Token::Category::END_OF_LINE:
	case Token::Category::SEMICOLON:
		astLeafCreate();
		nextToken();
		break;
	default:
		RECURSE(AssignStatementParser, "In assign statement: syntax error");
		switch (readToken().category)
		{
		case Token::Category::RPAREN:
			ERROR("Redundant ')' after expr");
		case Token::Category::RBRACKET:
			ERROR("Redundant ']' after expr");
		case Token::Category::RBRACE:
			ERROR("Redundant '}' after expr");
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
	RECURSE(LeftValueExpressionParser, "In LHS of assign statement: syntax error");
	goto ASG;
}

ASG: {
	switch (readToken().category)
	{
	case Token::Category::ASG: case Token::Category::ADD_ASG: case Token::Category::SUB_ASG: case Token::Category::MUL_ASG: case Token::Category::DIV_ASG:	case Token::Category::MOD_ASG:	case Token::Category::BIT_AND_ASG:	case Token::Category::BIT_OR_ASG: case Token::Category::BIT_XOR_ASG: case Token::Category::SHL_ASG:	case Token::Category::ASHR_ASG:
		astLeafCreate();
		nextToken();
		goto RIGHT;
	case Token::Category::COMMA: // 
		astLeafCreate();
		nextToken();
		goto LEFT;
	default:
		ERROR("In assign statement: missing assigning operator")
	}
}

RIGHT: {
	RECURSE(RightValueExpressionParser, "In RHS of assign statement: syntax error");
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

	RECURSE(RightValueExpressionParser, "In IF condition: syntax error");

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
		ERROR("In function defination: incorrect ending of IF statement")
	case Token::Category::ELSE:
		astNodeReduce(AST::Category::IF_BODY, if_body);
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IF)
		{
			RECURSE(IfStatementParser, "In IF statement: syntax error");
			goto SUCCESS; // end if
		}
		goto ELSE_BODY;
	default:
		if_body++;
		RECURSE(StatementParser, "In IF statement: syntax error");
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
		ERROR("In function defination: incorrect ending of ELSE statement")
	default:
		else_body++;
		RECURSE(StatementParser, "In ELSE statement: syntax error");
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
		ERROR("In loop defination: assignment not found")
	}
	ERROR("In loop defination: loop counter not found")
}

DO_HEAD: {
	RECURSE(RightValueExpressionParser, "In loop condition: starting of loop counter not found");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_HEAD, 2);
		goto DO_TAIL;
	}
	ERROR("In loop condition: incorrect ending of loop counter")
}

DO_TAIL: {
	RECURSE(RightValueExpressionParser, "In loop conditon: ending of loop counter not found");
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
	ERROR("In loop condition: incorrect stride of loop counter")
}

DO_STEP: {
	RECURSE(RightValueExpressionParser, "In loop condition: stride of loop counter not found");
	astNodeReduce(AST::Category::LOOP_STEP, 1);
	if (readToken().category == Token::Category::END_OF_LINE)
	{
		goto DO_BODY;
	}
	ERROR("In loop condition: incorrect loop body")
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
		ERROR("In function defination: incorrect ending of loop statement")
	default:
		doBody++;
		RECURSE(StatementParser, "In statement: syntax error");
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
		ERROR("In loop defination: assignment not found")
	}
	ERROR("In loop defination: loop counter not found")
}

FOR_HEAD: {
	RECURSE(RightValueExpressionParser, "In loop condition: starting of loop counter not found");
	if (readToken().category == Token::Category::COMMA)
	{
		astLeafCreate();
		nextToken();
		astNodeReduce(AST::Category::LOOP_HEAD, 2);
		goto FOR_TAIL;
	}
	ERROR("In loop condition: incorrect ending of loop counter")
}

FOR_TAIL: {
	RECURSE(RightValueExpressionParser, "In loop conditon: ending of loop counter not found");
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
	ERROR("In loop condition: incorrect stride of loop counter")
}

FOR_STEP: {
	RECURSE(RightValueExpressionParser, "In loop condition: stride of loop counter not found");
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
	ERROR("In loop condition: incorrect loop body")
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
		ERROR("In function defination: incorrect ending of loop statement")
	default:
		forBody++;
		RECURSE(StatementParser, "In statement: syntax error");
		goto FOR_BODY;
	}
}

SUCCESS:
	astNodeReduce(AST::Category::FOR_STATEMENT);
	return std::move(astStack.back());
}
