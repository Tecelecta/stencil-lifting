#pragma once

#include "Token.h"

#include <vector>
#include <memory>

struct AST
{
	enum class Category : size_t
	{
		TERMINAL,				//!< 终结符，是语法树的叶子结点
		COMPILE_UNIT,			//!< 编译单元，是语法树的根结点

		FUNCTION_DEFINITION,	//!< 函数定义
		FUNCTION_RETURN_VALUE,	//!< 函数返回值定义
		FUNCTION_NAME,			//!< 函数名
		FUNCTION_PARAMETER,		//!< 函数形参
		FUNCTION_BODY,			//!< 函数体

		VARIABLE_DEFINITION,	//!< 变量定义
		CONST_DEFINITION,		//!< 常量定义

		STRUCT_DEFINITION,		//!< 结构体定义
		STRUCT_BODY,			//!< 结构体定义主体部分

		IF_STATEMENT,			//!< 条件语句
		IF_BODY,				//!< if分支的语句块
		ELSE_BODY,				//!< else分支的语句块

		DO_STATEMENT,			//!< 串行循环语句
		FOR_STATEMENT,			//!< 并行循环语句
		LOOP_INDEX,				//!< 循环下标声明
		LOOP_VARIABLE,			//!< 循环变量
		LOOP_HEAD,				//!< 循环开始
		LOOP_TAIL,				//!< 循环结束
		LOOP_STEP,				//!< 循环步长
		LOOP_BODY,				//!< 循环体

		ASSIGN_STATEMENT,		//!< 赋值语句

		UNARY_EXPRESSION,		//!< 一元运算符表达式
		BINARY_EXPRESSION,		//!< 一元运算符表达式

		PRIMARY_EXPRESSION,		//!< 被括起来优先计算的表达式

		FUNCTION_CALL,			//!< 函数调用
		ARGUMENT_LIST,			//!< 实参列表

		ARRAY_ELEMENT,			//!< 数组元素
		ARRAY_SUBSCRIPT,		//!< 数组下标

		STRUCT_ELEMENT,			//!< 结构体元素

		TUPLE_INITIALIZER,		//!< 元组初始化

	} category = Category::TERMINAL;
	Token firstToken;
	AST* father = nullptr;
	std::vector<std::unique_ptr<AST>> children;
};

/**
	* @brief 递归调用，打印AST
	* @param ast 要打印的子树
	* @param indent 缩进级别
	*/
SDSL_API void printASTNode(const AST* ast, size_t indent = 0);

SDSL_API const char* getName(AST::Category _enum);
