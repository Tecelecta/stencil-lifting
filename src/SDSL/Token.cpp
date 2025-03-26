#define SDSL_EXPORTS

#include "Token.h"

const char* getName(Token::Category _enum)
{
	switch (_enum)
	{
	case Token::Category::END_OF_LINE: return "END_OF_LINE";
	case Token::Category::END_OF_FILE: return "END_OF_FILE";
	case Token::Category::IDENTIFIER: return "IDENTIFIER";
	case Token::Category::VAR: return "VAR";
	case Token::Category::CONST: return "CONST";
	case Token::Category::IF: return "IF";
	case Token::Category::ELSE: return "ELSE";
	case Token::Category::DO: return "DO";
	case Token::Category::FOR: return "FOR";
	case Token::Category::FUNC: return "FUNC";
	case Token::Category::STRUCT: return "STRUCT";
	case Token::Category::END: return "END";
	case Token::Category::COLON: return "COLON";
	case Token::Category::SEMICOLON: return "SEMICOLON";
	case Token::Category::COMMA: return "COMMA";
	case Token::Category::DOT: return "DOT";
	case Token::Category::QUESTION: return "QUESTION";
	case Token::Category::LPAREN: return "LPAREN";
	case Token::Category::RPAREN: return "RPAREN";
	case Token::Category::LBRACKET: return "LBRACKET";
	case Token::Category::RBRACKET: return "RBRACKET";
	case Token::Category::LBRACE: return "LBRACE";
	case Token::Category::RBRACE: return "RBRACE";
	case Token::Category::ASG: return "ASG";
	case Token::Category::ADD: return "ADD";
	case Token::Category::SUB: return "SUB";		
	case Token::Category::MUL: return "MUL";		
	case Token::Category::DIV: return "DIV";	
	case Token::Category::MOD: return "MOD";		
	case Token::Category::NOT: return "NOT";		
	case Token::Category::AND: return "AND";		
	case Token::Category::OR: return "OR";			
	case Token::Category::BIT_AND: return "BIT_AND";	
	case Token::Category::BIT_OR: return "BIT_OR";		
	case Token::Category::BIT_XOR: return "BIT_XOR";	
	case Token::Category::BIT_NOT: return "BIT_NOT";	
	case Token::Category::SHL: return "SHL";	
	case Token::Category::ASHR: return "ASHR";	
	case Token::Category::LSHR: return "LSHR";	
	case Token::Category::POW: return "POW";	
	case Token::Category::EQUAL: return "EQUAL";	
	case Token::Category::NOT_EQUAL: return "NOT_EQUAL";	
	case Token::Category::LES: return "LES";		
	case Token::Category::GRT: return "GRT";		
	case Token::Category::LES_EQUAL: return "LES_EQUAL";	
	case Token::Category::GRT_EQUAL: return "GRT_EQUAL";	
	case Token::Category::ADD_ASG: return "ADD_ASG";		
	case Token::Category::SUB_ASG: return "SUB_ASG";		
	case Token::Category::MUL_ASG: return "MUL_ASG";		
	case Token::Category::DIV_ASG: return "DIV_ASG";		
	case Token::Category::MOD_ASG: return "MOD_ASG";		
	case Token::Category::BIT_AND_ASG: return "BIT_AND_ASG";	
	case Token::Category::BIT_OR_ASG: return "BIT_OR_ASG";
	case Token::Category::BIT_XOR_ASG: return "BIT_XOR_ASG";	
	case Token::Category::SHL_ASG: return "SHL_ASG";	
	case Token::Category::ASHR_ASG: return "ASHR_ASG";	
	case Token::Category::LSHR_ASG: return "LSHR_ASG";	
	case Token::Category::DEC_INTEGER: return "DEC_INTEGER";
	case Token::Category::BIN_INTEGER: return "BIN_INTEGER";
	case Token::Category::HEX_INTEGER: return "HEX_INTEGER";
	case Token::Category::DEC_RATIONAL: return "DEC_RATIONAL";
	case Token::Category::ANNOTATION: return "ANNOTATION";
	default: return "ERROR";
	}
}
