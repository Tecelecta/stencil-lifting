#pragma once

#include "Token.h"

#include <vector>
#include <memory>

struct AST
{
	enum class Category : size_t
	{
		TERMINAL,				//!< 
		COMPILE_UNIT,			//!< 

		FUNCTION_DEFINITION,	//!< 
		FUNCTION_RETURN_VALUE,	//!< 
		FUNCTION_NAME,			//!< 
		FUNCTION_PARAMETER,		//!< 
		FUNCTION_BODY,			//!< 

		VARIABLE_DEFINITION,	//!< 
		CONST_DEFINITION,		//!< 

		STRUCT_DEFINITION,		//!< 
		STRUCT_BODY,			//!< 

		IF_STATEMENT,			//!< 
		IF_BODY,				//!< if
		ELSE_BODY,				//!< else

		DO_STATEMENT,			//!< 
		FOR_STATEMENT,			//!< 
		LOOP_INDEX,				//!< 
		LOOP_VARIABLE,			//!< 
		LOOP_HEAD,				//!< 
		LOOP_TAIL,				//!< 
		LOOP_STEP,				//!< 
		LOOP_BODY,				//!< 

		ASSIGN_STATEMENT,		//!< 

		UNARY_EXPRESSION,		//!< 
		BINARY_EXPRESSION,		//!< 

		PRIMARY_EXPRESSION,		//!< 

		FUNCTION_CALL,			//!< 
		ARGUMENT_LIST,			//!< 

		ARRAY_ELEMENT,			//!< 
		ARRAY_SUBSCRIPT,		//!< 

		STRUCT_ELEMENT,			//!< 

		TUPLE_INITIALIZER,		//!< 

	} category = Category::TERMINAL;
	Token firstToken;
	AST* father = nullptr;
	std::vector<std::unique_ptr<AST>> children;
};

/**
	* @brief AST
	* @param ast 
	* @param indent 
	*/
SDSL_API void printASTNode(const AST* ast, size_t indent = 0);

SDSL_API const char* getName(AST::Category _enum);
