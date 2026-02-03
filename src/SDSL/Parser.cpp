#define PARSER_CPP
#define SDSL_EXPORTS

#include "Parser.h"

#include <cassert>

void Parser::setup(std::queue<Token>& tokenStream)
{
	tokenStreamPtr = &tokenStream;
}

void Parser::nextToken()
{
	tokenStreamPtr->pop();
}

// 将当前的token转换为AST节点，压入astStack
void Parser::astLeafCreate()
{
	auto ast = std::make_unique<AST>();
	ast->firstToken = readToken();
	astStack.push_back(std::move(ast));
}

// 将astStack中全部的节点合并成category类型的父节点，压入astStack
void Parser::astNodeReduce(AST::Category category)
{
	astNodeReduce(category, astStack.size());
}

// 将astStack中num个AST的节点合并成category类型的父节点，压入astStack
void Parser::astNodeReduce(AST::Category category, size_t num)
{
	assert(num <= astStack.size());
	auto ast = std::make_unique<AST>();
	ast->category = category;
	if (num > 0)
	{
		ast->children.resize(num);
		for (size_t i = 0; i < num; i++)
		{
			ast->children[i] = std::move(astStack[astStack.size() - num + i]);
			ast->children[i]->father = ast.get();
		}
		ast->firstToken = ast->children.front()->firstToken;
	}
	astStack.resize(astStack.size() - num + 1);
	astStack.back() = std::move(ast);
}

std::unique_ptr<AST> CompileUnitParser::run(std::queue<Token>& tokenStream)
{
	setup(tokenStream);
	while (true)
	{
		switch (readToken().category)
		{
		case Token::Category::END_OF_FILE:
			astLeafCreate();
			astNodeReduce(AST::Category::COMPILE_UNIT);
			return std::move(astStack.back());
		case Token::Category::END_OF_LINE:
			astLeafCreate();
			nextToken();
			break;
		case Token::Category::FUNC:
			RECURSE(FunctionDefinitionParser, "In function defination: syntax error");
			break;
		case Token::Category::CONST:
			RECURSE(VariableDefinitionParser, "In global constant defination: syntax error");
			break;
		case Token::Category::STRUCT:
			RECURSE(StructDefinitionParser, "In structure defination: syntax error");
			break;
		case Token::Category::VAR:
			ERROR("Global variable defination is not allowed")
		default:
			ERROR("Unexpected token")
		}
	}
}
