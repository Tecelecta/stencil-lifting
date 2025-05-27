#pragma once

#include "SDSL_global.h"

#include <string>

struct Token
{
	/// 类别
	enum class Category : size_t
	{
		// 基本
		END_OF_LINE,	//!< 换行符
		END_OF_FILE,	//!< 文件结束
		IDENTIFIER,		//!< 标识符：[_a-zA-Z][_a-zA-Z0-9]*

		// 关键字 KEYWORD
		VAR,			//!< 变量定义：var
		CONST,			//!< 常量定义：const
		IF,				//!< 开始分支：if
		ELSE,			//!< 其他分支：else
		DO,				//!< 串行循环：do
		FOR,			//!< 并行循环：for
		FUNC,			//!< 函数定义：func
		STRUCT,			//!< 结构体定义：struct
		END,			//!< 块结束：end

		// 标点符号 PUNCTUATION ()[]{},.;?
		COLON,			//!< 冒号（:）
		SEMICOLON,		//!< 分号（;）
		COMMA,			//!< 逗号（,）
		DOT,			//!< 点号（.）
		QUESTION,		//!< 问号（?）
		LPAREN,			//!< 左小括号（(）
		RPAREN,			//!< 右小括号（)）
		LBRACKET,		//!< 左中括号（[）
		RBRACKET,		//!< 右中括号（]）
		LBRACE,			//!< 左大括号（{）
		RBRACE,			//!< 右大括号（}）

		// 运算符 OPERATOR
		ASG,			//!< 赋值（=）
		ADD,			//!< 加号（+）
		SUB,			//!< 减号（-）
		MUL,			//!< 乘号（*）
		DIV,			//!< 除号（/）
		MOD,			//!< 取模（%）
		NOT,			//!< 逻辑非（!）
		AND,			//!< 短路逻辑与（&&）
		OR,				//!< 短路逻辑或（||）
		BIT_AND,		//!< 按位与（&）
		BIT_OR,			//!< 按位或（|）
		BIT_XOR,		//!< 按位异或（^）
		BIT_NOT,		//!< 按位取反（~）
		SHL,			//!< 左移（<<）
		ASHR,			//!< 算术右移（>>）
		LSHR,			//!< 逻辑右移（>>>）
		POW,			//!< 乘方（**）
		EQUAL,			//!< 判断相等（==）
		NOT_EQUAL,		//!< 判断不等（!=）
		LES,			//!< 判断小于（<）
		GRT,			//!< 判断大于（>）
		LES_EQUAL,		//!< 判断小于等于（<=）
		GRT_EQUAL,		//!< 判断大于等于（>=）
		ADD_ASG,		//!< （+=）
		SUB_ASG,		//!< （-=）
		MUL_ASG,		//!< （*=）
		DIV_ASG,		//!< （/=）
		MOD_ASG,		//!< （%=）
		BIT_AND_ASG,	//!< （&=）
		BIT_OR_ASG,		//!< （|=）
		BIT_XOR_ASG,	//!< （^=）
		SHL_ASG,		//!< （<<=）
		ASHR_ASG,		//!< （>>=）
		LSHR_ASG,		//!< （>>>=）

		// 数值字面量
		DEC_INTEGER,	//!< 十进制整数
		BIN_INTEGER,	//!< 二进制整数
		HEX_INTEGER,	//!< 十六进制整数
		DEC_RATIONAL,	//!< 十进制有理数

		// 辅助
		ANNOTATION,			//!< @开头的注解

	} category = Category::END_OF_LINE;
	size_t line = 0; //!< 行号
	size_t column = 0; //!< 列号
	std::string_view text; //!< 文本

	bool isKeyword() const
	{
		return static_cast<size_t>(category) >= static_cast<size_t>(Category::VAR)
			&& static_cast<size_t>(category) >= static_cast<size_t>(Category::END);
	}
};

SDSL_API const char* getName(Token::Category _enum);
