#define SDSL_EXPORTS

#include "AST.h"
#include <iostream>

void printASTNode(const AST* ast, size_t indent)
{
	if (!ast)
	{
		return;
	}
	for (size_t i = 0; i < indent; i++)
	{
		std::cout << "   ";
	}
	std::cout
		<< "\033[3" << std::to_string((indent) % 6 + 1) << "m" << getName(ast->category) << "\033[0m: "
		<< ast->firstToken.text << " "
		<< "<Line : " << ast->firstToken.line
		<< ", Col : " << ast->firstToken.column << ">\n";
	for (auto& child : ast->children)
	{
		printASTNode(child.get(), indent + 1);
	}
}

const char* getName(AST::Category _enum)
{
	switch (_enum)
	{
	case AST::Category::TERMINAL: return "TERMINAL";
	case AST::Category::COMPILE_UNIT: return "COMPILE_UNIT";
	case AST::Category::FUNCTION_DEFINITION: return "FUNCTION_DEFINITION";
	case AST::Category::FUNCTION_RETURN_VALUE: return "FUNCTION_RETURN_VALUE";
	case AST::Category::FUNCTION_NAME: return "FUNCTION_NAME";
	case AST::Category::FUNCTION_PARAMETER: return "FUNCTION_PARAMETER";
	case AST::Category::FUNCTION_BODY: return "FUNCTION_BODY";
	case AST::Category::VARIABLE_DEFINITION: return "VARIABLE_DEFINITION";
	case AST::Category::CONST_DEFINITION: return "CONST_DEFINITION";
	case AST::Category::STRUCT_DEFINITION: return "STRUCT_DEFINITION";
	case AST::Category::STRUCT_BODY: return "STRUCT_BODY";
	case AST::Category::IF_STATEMENT: return "IF_STATEMENT";
	case AST::Category::IF_BODY: return "IF_BODY";
	case AST::Category::ELSE_BODY: return "ELSE_BODY";
	case AST::Category::DO_STATEMENT: return "DO_STATEMENT";
	case AST::Category::FOR_STATEMENT: return "FOR_STATEMENT";
	case AST::Category::LOOP_INDEX: return "LOOP_INDEX";
	case AST::Category::LOOP_VARIABLE: return "LOOP_VARIABLE";
	case AST::Category::LOOP_HEAD: return "LOOP_HEAD";
	case AST::Category::LOOP_TAIL: return "LOOP_TAIL";
	case AST::Category::LOOP_STEP: return "LOOP_STEP";
	case AST::Category::LOOP_BODY: return "LOOP_BODY";
	case AST::Category::ASSIGN_STATEMENT: return "ASSIGN_STATEMENT";
	case AST::Category::UNARY_EXPRESSION: return "UNARY_EXPRESSION";
	case AST::Category::BINARY_EXPRESSION: return "BINARY_EXPRESSION";
	case AST::Category::PRIMARY_EXPRESSION: return "PRIMARY_EXPRESSION";
	case AST::Category::FUNCTION_CALL: return "FUNCTION_CALL";
	case AST::Category::ARGUMENT_LIST: return "ARGUMENT_LIST";
	case AST::Category::ARRAY_ELEMENT: return "ARRAY_ELEMENT";
	case AST::Category::ARRAY_SUBSCRIPT: return "ARRAY_SUBSCRIPT";
	case AST::Category::STRUCT_ELEMENT: return "STRUCT_ELEMENT";
	case AST::Category::TUPLE_INITIALIZER: return "TUPLE_INITIALIZER";
	default: return "ERROR";
	}
}
