#define PARSER_CPP
#include "Parser.h"

#define CONSTANT_NUMERAL DEC_INTEGER: case Token::Category::BIN_INTEGER: case Token::Category::HEX_INTEGER: case Token::Category::DEC_RATIONAL
#define RELATION_OPERATOR LES: case Token::Category::LES_EQUAL:	case Token::Category::GRT: case Token::Category::GRT_EQUAL:	case Token::Category::EQUAL: case Token::Category::NOT_EQUAL
#define FOLLOW_SET END_OF_LINE: case Token::Category::SEMICOLON: case Token::Category::COMMA: case Token::Category::COLON: case Token::Category::RBRACE: case Token::Category::RBRACKET: case Token::Category::RPAREN
#define COMPUTE_ASG ADD_ASG: case Token::Category::SUB_ASG: case Token::Category::MUL_ASG: case Token::Category::DIV_ASG:	case Token::Category::MOD_ASG:	case Token::Category::BIT_AND_ASG:	case Token::Category::BIT_OR_ASG: case Token::Category::BIT_XOR_ASG: case Token::Category::SHL_ASG:	case Token::Category::ASHR_ASG

std::unique_ptr<AST> LeftValueExpressionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	goto START;

START: {
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
		astLeafCreate();
		nextToken();
		goto LEFT_VALUE;
	case Token::Category::CONSTANT_NUMERAL:
		ERROR("立即数不能作为左值")
	default:
		ERROR("在分析左值表达式时，未发现左值")
	}
}

LEFT_VALUE: {
	switch (readToken().category)
	{
	case Token::Category::LBRACKET:
		RECURSE(ArraySubscriptExpressionParser, "在分析数组表达式中，发现语法错误");
		astNodeReduce(AST::Category::ARRAY_ELEMENT, 2);
		goto LEFT_VALUE;
	case Token::Category::DOT:
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IDENTIFIER)
		{
			astLeafCreate();
			nextToken();
			astNodeReduce(AST::Category::STRUCT_ELEMENT, 3);
			goto LEFT_VALUE;
		}
		ERROR("在分析结构体表达式中，结构体成员不是标识符")
	case Token::Category::ASG:
	case Token::Category::COMPUTE_ASG:
	case Token::Category::COMMA:
		goto SUCCESS;
	default:
		ERROR("在分析左值表达式中，发现不支持的符号")
	}
}

SUCCESS:
	return std::move(astStack.back());
}

std::unique_ptr<AST> RightValueExpressionParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);

	enum class State
	{
		START,

		ID,

		LPAREN,

		UN,
		UN_ID,

		ID_ADD,
		ID_ADD_ID,

		ID_MUL,
		ID_MUL_ID,

		ID_POW,
		ID_POW_ID,

		ID_SF,
		ID_SF_ID,

		ID_REL,
		ID_REL_ID,

		ID_LOR,
		ID_LOR_ID,

		ID_LAND,
		ID_LAND_ID,

		ID_BOR,
		ID_BOR_ID,

		ID_BXOR,
		ID_BXOR_ID,

		ID_BAND,
		ID_BAND_ID,
	};

	std::vector<State> stateStack;
	goto START;

START: {
	stateStack.push_back(State::START);
	switch (readToken().category)
	{
	case Token::Category::LBRACE:
		RECURSE(ArgumentListExpressionParser, "在分析参数列表表达式时，元组初始化的语法错误");
		goto COMPLETE_EXPR;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	default:
		ERROR("START")
	}
}

ID: {
	stateStack.push_back(State::ID);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		ERROR("在分析右值表达式时，表达式连续出现两个标识符或立即数")
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
		astLeafCreate();
		nextToken();
		goto ID_MUL;
	case Token::Category::ADD: case Token::Category::SUB:
		astLeafCreate();
		nextToken();
		goto ID_ADD;
	case Token::Category::SHL: case Token::Category::ASHR:
		astLeafCreate();
		nextToken();
		goto ID_SF;
	case Token::Category::RELATION_OPERATOR:
		astLeafCreate();
		nextToken();
		goto ID_REL;
	case Token::Category::BIT_AND:
		astLeafCreate();
		nextToken();
		goto ID_BAND;
	case Token::Category::BIT_XOR:
		astLeafCreate();
		nextToken();
		goto ID_BXOR;
	case Token::Category::BIT_OR:
		astLeafCreate();
		nextToken();
		goto ID_BOR;
	case Token::Category::AND:
		astLeafCreate();
		nextToken();
		goto ID_LAND;
	case Token::Category::OR:
		astLeafCreate();
		nextToken();
		goto ID_LOR;
	case Token::Category::FOLLOW_SET:
		goto COMPLETE_EXPR;
	default:
		ERROR("ID")
	}
}

LPAREN:  { // START
	stateStack.push_back(State::LPAREN);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto LPAREN_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	default:
		ERROR("LPAREN")
	}
}

LPAREN_ID: {
	// 这是一个判断是否进入规约态的过渡状态，不压栈！
	if (readToken().category == Token::Category::RPAREN)
	{
		astLeafCreate();
		nextToken();
		goto LPAREN_ID_RPAREN;
	}
	else
	{
		goto ID;
	}
}

LPAREN_ID_RPAREN: {
	// 这是一个进行规约然后弹栈的状态，不压栈！
	astNodeReduce(AST::Category::PRIMARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 1);
	switch (stateStack.back())
	{
	case State::UN:
		goto UN_ID;
	case State::ID_POW:
		goto ID_POW_ID;
	case State::ID_MUL:
		goto ID_MUL_ID;
	case State::ID_ADD:
		goto ID_ADD_ID;
	case State::ID_SF:
		goto ID_SF_ID;
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_BAND:
		goto ID_BAND_ID;
	case State::ID_BXOR:
		goto ID_BXOR_ID;
	case State::ID_BOR:
		goto ID_BOR_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::START:
		goto ID;
	case State::LPAREN:
		goto LPAREN_ID;
	default:
		ERROR("LPAREN_ID_RPAREN")
	}
}

ID_ID: {
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
		RECURSE(ArgumentListExpressionParser, "在实参列表表达式中发现语法错误");
		astNodeReduce(AST::Category::FUNCTION_CALL, 2);
		goto ID_ID_EXPR;
	case Token::Category::LBRACKET:
		RECURSE(ArraySubscriptExpressionParser, "在数组表达式中发现语法错误");
		astNodeReduce(AST::Category::ARRAY_ELEMENT, 2);
		goto ID_ID_EXPR;
	case Token::Category::DOT:
		astLeafCreate();
		nextToken();
		if (readToken().category == Token::Category::IDENTIFIER)
		{
			astLeafCreate();
			nextToken();
			astNodeReduce(AST::Category::STRUCT_ELEMENT, 3);
			goto ID_ID_EXPR;
		}
		ERROR("STRUCT_ELEMENT")
	default:
		ERROR("ID_ID")
	}
}

ID_ID_EXPR: {
	stateStack.resize(stateStack.size() - 1);
	switch (stateStack.back())
	{
	case State::UN:
		goto UN_ID;
	case State::ID_POW:
		goto ID_POW_ID;
	case State::ID_MUL:
		goto ID_MUL_ID;
	case State::ID_ADD:
		goto ID_ADD_ID;
	case State::ID_SF:
		goto ID_SF_ID;
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_BAND:
		goto ID_BAND_ID;
	case State::ID_BXOR:
		goto ID_BXOR_ID;
	case State::ID_BOR:
		goto ID_BOR_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto ID;
	default:
		ERROR("ID_ID_EXPR")
	}
}

UN: {
	stateStack.push_back(State::UN);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto UN_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	default:
		ERROR("UN")
	}
}

UN_ID: {
	stateStack.push_back(State::UN_ID);
	switch (readToken().category)
	{
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::POW:
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
	case Token::Category::ADD: case Token::Category::SUB:
	case Token::Category::SHL: case Token::Category::ASHR:
	case Token::Category::RELATION_OPERATOR:
	case Token::Category::BIT_AND: case Token::Category::BIT_OR: case Token::Category::BIT_XOR:
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto UN_EXPR;
	default:
		ERROR("UN_ID")
	}
}

UN_EXPR: {
	astNodeReduce(AST::Category::UNARY_EXPRESSION, 2);
	stateStack.resize(stateStack.size() - 2);
	switch (stateStack.back())
	{
	case State::ID_POW:
		goto ID_POW_ID;
	case State::ID_MUL:
		goto ID_MUL;
	case State::ID_ADD:
		goto ID_ADD_ID;
	case State::ID_SF:
		goto ID_SF_ID;
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_BAND:
		goto ID_BAND_ID;
	case State::ID_BXOR:
		goto ID_BXOR_ID;
	case State::ID_BOR:
		goto ID_BOR_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto ID;
	default:
		ERROR("UN_EXPR")
	}
}

ID_POW: {
	stateStack.push_back(State::ID_POW);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_POW_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("乘方符号'**'后有不完整的括号表达式")
	default:
		ERROR("ID_POW")
	}
}

ID_POW_ID: {
	stateStack.push_back(State::ID_POW_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
	case Token::Category::ADD: case Token::Category::SUB:
	case Token::Category::RELATION_OPERATOR:
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto POW_EXPR;
	default:
		ERROR("ID_POW_ID")
	}
}

POW_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_POW:
		goto ID_POW_ID;
	case State::ID_MUL:
		goto ID_MUL_ID;
	case State::ID_ADD:
		goto ID_ADD_ID;
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("POW_EXPR")
	}
}

ID_MUL: {
	stateStack.push_back(State::ID_MUL);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_MUL_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("乘法除法取模运算'*/%'后有不完整的括号表达式")
	default:
		ERROR("ID_MUL")
	}
}

ID_MUL_ID: {
	stateStack.push_back(State::ID_MUL_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
	case Token::Category::ADD: case Token::Category::SUB:
	case Token::Category::RELATION_OPERATOR:
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto MUL_EXPR;
	default:
		ERROR("ID_MUL_ID")
	}
}

MUL_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_ADD:
		goto ID_ADD_ID;
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("MUL_EXPR")
	}
}

ID_ADD: {
	stateStack.push_back(State::ID_ADD);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_ADD_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("加法减法运算'+-'后有不完整的括号表达式")
	default:
		ERROR("ID_ADD")
	}
}

ID_ADD_ID: {
	stateStack.push_back(State::ID_ADD_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
		astLeafCreate();
		nextToken();
		goto ID_MUL;
	case Token::Category::ADD: case Token::Category::SUB:
	case Token::Category::RELATION_OPERATOR:
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto ADD_EXPR;
	default:
		ERROR("ID_ADD_ID")
	}
}

ADD_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_REL:
		goto ID_REL_ID;
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("ADD_EXPR")
	}
}

ID_SF: {
	stateStack.push_back(State::ID_SF);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_SF_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("移位运算'<<''>>'后有不完整的括号表达式")
	default:
		ERROR("ID_SF")
	}
}

ID_SF_ID: {
	stateStack.push_back(State::ID_SF_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::SHL:	case Token::Category::ASHR:
	case Token::Category::FOLLOW_SET:
		goto SF_EXPR;
	default:
		ERROR("ID_SF_ID")
	}
}

SF_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("BXOR_EXPR")
	}
}

ID_REL: {
	stateStack.push_back(State::ID_REL);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_REL_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("关系运算'<''<=''>''>=''==''!='后有不完整的括号表达式")
	default:
		ERROR("ID_REL")
	}
}

ID_REL_ID: {
	stateStack.push_back(State::ID_REL_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
		astLeafCreate();
		nextToken();
		goto ID_MUL;
	case Token::Category::ADD: case Token::Category::SUB:
		astLeafCreate();
		nextToken();
		goto ID_ADD;
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto REL_EXPR;
	default:
		ERROR("ID_REL_ID")
	}
}

REL_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_LAND:
		goto ID_LAND_ID;
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("REL_EXPR")
	}
}

ID_BAND: {
	stateStack.push_back(State::ID_BAND);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_BAND_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("位与运算'&'后有不完整的括号表达式")
	default:
		ERROR("ID_BAND")
	}
}

ID_BAND_ID: {
	stateStack.push_back(State::ID_BAND_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::BIT_AND: case Token::Category::BIT_XOR: case Token::Category::BIT_OR:
	case Token::Category::FOLLOW_SET:
		goto BAND_EXPR;
	default:
		ERROR("ID_BAND_ID")
	}
}

BAND_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_BXOR:
		goto ID_BXOR_ID;
	case State::ID_BOR:
		goto ID_BOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("BAND_EXPR")
	}
}

ID_BXOR: {
	stateStack.push_back(State::ID_BXOR);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_BXOR_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("位异或运算'^'后有不完整的括号表达式")
	default:
		ERROR("ID_BXOR")
	}
}

ID_BXOR_ID: {
	stateStack.push_back(State::ID_BXOR_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::BIT_AND:
		astLeafCreate();
		nextToken();
		goto ID_BAND;
	case Token::Category::BIT_XOR:	case Token::Category::BIT_OR:
	case Token::Category::FOLLOW_SET:
		goto BXOR_EXPR;
	default:
		ERROR("ID_BXOR_ID")
	}
}

BXOR_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_BOR:
		goto ID_BOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("BXOR_EXPR")
	}
}

ID_BOR: {
	stateStack.push_back(State::ID_BOR);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_BOR_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("位或运算'|'后有不完整的括号表达式")
	default:
		ERROR("ID_BOR")
	}
}

ID_BOR_ID: {
	stateStack.push_back(State::ID_BOR_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::BIT_AND:
		astLeafCreate();
		nextToken();
		goto ID_BAND;
	case Token::Category::BIT_XOR:
		astLeafCreate();
		nextToken();
		goto ID_BXOR;
	case Token::Category::BIT_OR:
	case Token::Category::FOLLOW_SET:
		goto BOR_EXPR;
	default:
		ERROR("ID_BOR_ID")
	}
}

BOR_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("BOR_EXPR")
	}
}

ID_LAND: {
	stateStack.push_back(State::ID_LAND);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_LAND_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("逻辑与运算'&&'后有不完整的括号表达式")
	default:
		ERROR("ID_LAND")
	}
}

ID_LAND_ID: {
	stateStack.push_back(State::ID_LAND_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
		astLeafCreate();
		nextToken();
		goto ID_MUL;
	case Token::Category::ADD: case Token::Category::SUB:
		astLeafCreate();
		nextToken();
		goto ID_ADD;
	case Token::Category::RELATION_OPERATOR:
		astLeafCreate();
		nextToken();
		goto ID_REL;
	case Token::Category::AND: case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto LAND_EXPR;
	default:
		ERROR("ID_LAND_ID")
	}
}

LAND_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::ID_LOR:
		goto ID_LOR_ID;
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("LAND_EXPR")
	}
}

ID_LOR: {
	stateStack.push_back(State::ID_LOR);
	switch (readToken().category)
	{
	case Token::Category::IDENTIFIER:
	case Token::Category::CONSTANT_NUMERAL:
		astLeafCreate();
		nextToken();
		goto ID_LOR_ID;
	case Token::Category::ADD: case Token::Category::SUB: case Token::Category::NOT: case Token::Category::BIT_NOT:
		astLeafCreate();
		nextToken();
		goto UN;
	case Token::Category::LPAREN:
		astLeafCreate();
		nextToken();
		goto LPAREN;
	case Token::Category::RPAREN:
		ERROR("逻辑或运算'||'后有不完整的括号表达式")
	default:
		ERROR("ID_LOR")
	}
}

ID_LOR_ID: {
	stateStack.push_back(State::ID_LOR_ID);
	switch (readToken().category)
	{
	case Token::Category::LPAREN:
	case Token::Category::LBRACKET:
	case Token::Category::DOT:
		goto ID_ID;
	case Token::Category::NOT: case Token::Category::BIT_NOT:
		ERROR("单目运算符不应出现在标识符右侧")
	case Token::Category::POW:
		astLeafCreate();
		nextToken();
		goto ID_POW;
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
		astLeafCreate();
		nextToken();
		goto ID_MUL;
	case Token::Category::ADD: case Token::Category::SUB:
		astLeafCreate();
		nextToken();
		goto ID_ADD;
	case Token::Category::RELATION_OPERATOR:
		astLeafCreate();
		nextToken();
		goto ID_REL;
	case Token::Category::AND:
		astLeafCreate();
		nextToken();
		goto ID_LAND;
	case Token::Category::OR:
	case Token::Category::FOLLOW_SET:
		goto LOR_EXPR;
	default:
		ERROR("ID_LOR_ID")
	}
}

LOR_EXPR: {
	astNodeReduce(AST::Category::BINARY_EXPRESSION, 3);
	stateStack.resize(stateStack.size() - 3);
	switch (stateStack.back())
	{
	case State::LPAREN:
		goto LPAREN_ID;
	case State::START:
		goto COMPLETE_EXPR;
	default:
		ERROR("LOR_EXPR")
	}
}

COMPLETE_EXPR: {
	switch (readToken().category)
	{
	case Token::Category::POW:
	case Token::Category::MUL: case Token::Category::DIV: case Token::Category::MOD:
	case Token::Category::ADD: case Token::Category::SUB:
	case Token::Category::SHL: case Token::Category::ASHR:
	case Token::Category::RELATION_OPERATOR:
	case Token::Category::BIT_AND: case Token::Category::BIT_XOR: case Token::Category::BIT_OR:
	case Token::Category::AND: case Token::Category::OR:
		goto ID;
	case Token::Category::FOLLOW_SET:
		goto SUCCESS;
	default:
		ERROR("表达式后存在多余的东西")
	}
}

SUCCESS:
	return std::move(astStack.back());
}
