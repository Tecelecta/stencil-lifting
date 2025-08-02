#pragma once

#include "SDSL_global.h"

#include <string>

struct Token
{
	/// 
	enum class Category : size_t
	{
		// 
		END_OF_LINE,	//!< 
		END_OF_FILE,	//!< 
		IDENTIFIER,		//!< [_a-zA-Z][_a-zA-Z0-9]*

		//  KEYWORD
		VAR,			//!< var
		CONST,			//!< const
		IF,				//!< if
		ELSE,			//!< else
		DO,				//!< do
		FOR,			//!< for
		FUNC,			//!< func
		STRUCT,			//!< struct
		END,			//!< end

		//  PUNCTUATION ()[]{},.;?
		COLON,			//!< :
		SEMICOLON,		//!< ;
		COMMA,			//!< ,
		DOT,			//!< .
		QUESTION,		//!< ?
		LPAREN,			//!< (
		RPAREN,			//!< )
		LBRACKET,		//!< [
		RBRACKET,		//!< ]
		LBRACE,			//!< {
		RBRACE,			//!< }

		//  OPERATOR
		ASG,			//!< =
		ADD,			//!< +
		SUB,			//!< -
		MUL,			//!< *
		DIV,			//!< /
		MOD,			//!< %
		NOT,			//!< !
		AND,			//!< &&
		OR,				//!< ||
		BIT_AND,		//!< &
		BIT_OR,			//!< |
		BIT_XOR,		//!< ^
		BIT_NOT,		//!< ~
		SHL,			//!< <<
		ASHR,			//!< >>
		LSHR,			//!< >>>
		POW,			//!< **
		EQUAL,			//!< ==
		NOT_EQUAL,		//!< !=
		LES,			//!< <
		GRT,			//!< >
		LES_EQUAL,		//!< <=
		GRT_EQUAL,		//!< >=
		ADD_ASG,		//!< +=
		SUB_ASG,		//!< -=
		MUL_ASG,		//!< *=
		DIV_ASG,		//!< /=
		MOD_ASG,		//!< %=
		BIT_AND_ASG,	//!< &=
		BIT_OR_ASG,		//!< |=
		BIT_XOR_ASG,	//!< ^=
		SHL_ASG,		//!< <<=
		ASHR_ASG,		//!< >>=
		LSHR_ASG,		//!< >>>=

		// 
		DEC_INTEGER,	//!< 
		BIN_INTEGER,	//!< 
		HEX_INTEGER,	//!< 
		DEC_RATIONAL,	//!< 

		// 
		ANNOTATION,			//!< @

	} category = Category::END_OF_LINE;
	size_t line = 0; //!< 
	size_t column = 0; //!< 
	std::string_view text; //!< 

	bool isKeyword() const
	{
		return static_cast<size_t>(category) >= static_cast<size_t>(Category::VAR)
			&& static_cast<size_t>(category) >= static_cast<size_t>(Category::END);
	}
};

SDSL_API const char* getName(Token::Category _enum);
