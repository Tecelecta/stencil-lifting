#define SDSL_EXPORTS

#include "Lexer.h"

#include <unordered_map>

#define ERROR(message) errorHandler.error(message, currLine, currColumn); return std::move(tokenStream); 
#define WARNING(message) errorHandler.warning(message, currLine, currColumn);
#define INFO(message) errorHandler.info(message, currLine, currColumn);

static const std::unordered_map<std::string_view, Token::Category> KEYWORD_MAP = {
	{ "var", Token::Category::VAR },
	{ "const", Token::Category::CONST },
	{ "if", Token::Category::IF },
	{ "else", Token::Category::ELSE },
	{ "do", Token::Category::DO },
	{ "for", Token::Category::FOR },
	{ "func", Token::Category::FUNC },
	{ "struct", Token::Category::STRUCT },
	{ "end", Token::Category::END },
};

std::queue<Token> Lexer::run(std::string_view code)
{
	// 处理空输入
	if (code.empty())
	{
		tokenEnd(Token::Category::END_OF_FILE);
		goto SUCCESS;
	}

	// 初始化输入状态，然后启动DFA
	this->code = code;
	currIndex = 0;
	currLine = 1;
	currColumn = 1;
	curr = code[0];

// DFA开始状态
START: {
	tokenBegin();
	switch (curr)
	{
	case '\0':
		tokenEnd(Token::Category::END_OF_FILE);
		goto SUCCESS;
	case ' ': case '\t':
		nextChar();
		goto START;
	case '\r':
		nextChar();
		if (config.endlCR)
		{
			tokenEnd(Token::Category::END_OF_LINE);
		}
		goto START;
	case '\n':
		nextChar();
		if (!config.endlCR)
		{
			tokenEnd(Token::Category::END_OF_LINE);
		}
		goto START;
	case '@':
		nextChar();
		goto ANNOTATION;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case '_':
		nextChar();
		goto IDENTIFIER;
	case '0':
		nextChar();
		goto ZERO;
	case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
		nextChar();
		goto DEC_INTEGER;
	case ':': case ';': case ',': case '.': case '?': case '(': case ')': case '[': case ']': case '{': case '}':
		goto PUNCTUATION;
	case '/': case '~':	case '^': case '%': case '=': case '!': case '*': case '+': case '-': case '&': case '|': case '<': case '>':
		goto OPERATOR;
	default:
		goto ILLEGAL;
	}
}

// 标识符
IDENTIFIER: {
	if (curr == '_' || isalpha(curr) || isdigit(curr))
	{
		nextChar();
		goto IDENTIFIER;
	}
	std::string_view word = code.substr(beginIndex, currIndex - beginIndex);
	auto iter = KEYWORD_MAP.find(word);
	if (iter != KEYWORD_MAP.end())
	{
		tokenEnd(iter->second);
	}
	else
	{
		tokenEnd(Token::Category::IDENTIFIER);
	}
	goto START;
}
	
// 注解
ANNOTATION: {
	if (curr == '_' || isalpha(curr) || isdigit(curr))
	{
		nextChar();
		goto ANNOTATION;
	}
	tokenEnd(Token::Category::ANNOTATION);
	goto START;
}

// 判断以0开头的数字：0 0. 0xX 0bB
ZERO: {
	if (curr == 'x' || curr == 'X')
	{
		nextChar();
		goto HEX_INTEGER;
	}
	else if (curr == 'b' || curr == 'B')
	{
		nextChar();
		goto BIN_INTEGER;
	}
	else if (curr == '.')
	{
		nextChar();
		goto FLOAT;
	}
	else if (isdigit(curr))
	{
		ERROR("Decimal part starting with 0")
	}
	else if (isalpha(curr))
	{
		ERROR("Idntifier starts with digit")
	}
	tokenEnd(Token::Category::DEC_INTEGER);
	goto START;
}

// 十进制整数
DEC_INTEGER: { // -> 0 | ([1-9][0-9]*) EXPONENT ->[eE][+-] ? [DIGIT]+
	if (isdigit(curr))
	{
		nextChar();
		goto DIGIT;
	}
	else if (curr == '.')
	{
		nextChar();
		goto FLOAT;
	}
	else if (curr == 'e' || curr == 'E')
	{
		nextChar();
		if (curr == '+' || curr == '-')
		{
			nextChar();
		}
		goto EXPFLOAT;
	}
	else if (isalpha(curr))
	{
		ERROR("Idntifier starts with digit")
	}
	tokenEnd(Token::Category::DEC_INTEGER);
	goto START;
}

// 以数字开头
DIGIT: { // -> [0-9]*
	if (isdigit(curr))
	{
		nextChar();
		goto DIGIT;
	}
	else if (curr == '.')
	{
		nextChar();
		goto FLOAT;
	}
	else if (curr == 'e' || curr == 'E')
	{
		nextChar();
		if (curr == '+' || curr == '-')
		{
			nextChar();
		}
		goto EXPFLOAT;
	}
	else if (isalpha(curr))
	{
		ERROR("Idntifier starts with digit")
	}
	tokenEnd(Token::Category::DEC_INTEGER);
	goto START;
}

// 十进制浮点数 -> DEC_INTEGER\.FRACTION ; FRACTION -> [DIGIT]+EXPONENT? ; EXPONENT ->[eE][+-] ? [DIGIT]+ ;
FLOAT: {
	if (isdigit(curr))
	{
		nextChar();
		goto FRACTION;
	}
	ERROR("小数部分出现Illegal token")
}
FRACTION: {
	if (isdigit(curr))
	{
		nextChar();
		goto FRACTION;
	}
	else if (curr == 'e' || curr == 'E')
	{
		nextChar();
		if (curr == '+' || curr == '-')
		{
			nextChar();
		}
		goto EXPFLOAT;
	}
	tokenEnd(Token::Category::DEC_RATIONAL);
	goto START;
}
EXPFLOAT: {
	if (isdigit(curr))
	{
		nextChar();
		goto EXPFLOATDIGIT;
	}
	else
	{
		ERROR("指数部分出现Illegal token")
	}
}
EXPFLOATDIGIT: {
	if (isdigit(curr))
	{
		nextChar();
		goto EXPFLOATDIGIT;
	}
	else if (isalpha(curr))
	{
		ERROR("指数部分出现Illegal token")
	}
	tokenEnd(Token::Category::DEC_RATIONAL);
	goto START;
}

// 十六进制整数
HEX_INTEGER: { // -> 0[xX][0-9a-fA-F]+
	if (isxdigit(curr)) // checks whether CURR is a hexdecimal DIGIT character
	{
		nextChar();
		goto HEXDIGIT;
	}
	ERROR("整数部分出现Illegal token")
}
HEXDIGIT: { // -> [0-9a-fA-F]*
	if (isxdigit(curr)) // checks whether CURR is a hexdecimal digit character
	{
		nextChar();
		goto HEXDIGIT;
	}
	else if (isalpha(curr))
	{
		ERROR("整数部分出现Illegal token")
	}
	tokenEnd(Token::Category::HEX_INTEGER);
	goto START;
}

// 二进制整数
BIN_INTEGER: { // -> 0[bB][01]+
	if (curr == '0' || curr == '1')
	{
		nextChar();
		goto BINDIGIT;
	}
	ERROR("整数部分出现Illegal token")
}
BINDIGIT: { // -> [01]*
	if (curr == '0' || curr == '1')
	{
		nextChar();
		goto BINDIGIT;
	}
	else if (isalnum(curr))
	{
		ERROR("整数部分出现Illegal token")
	}
	tokenEnd(Token::Category::BIN_INTEGER);
	goto START;
}

// 运算符
OPERATOR: {
	switch (curr)
	{
	case '/': // / /= /* //
		nextChar();
		if (curr == '/')
		{
			nextChar();
			goto COMMENT_SINGLELINE;
		}
		else if (curr == '*')
		{
			nextChar();
			goto COMMENT_MULTILINE;
		}
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::DIV_ASG);
			goto START;
		}
		tokenEnd(Token::Category::DIV);
		goto START;
	case '~':
		nextChar();
		tokenEnd(Token::Category::BIT_NOT);
		goto START;
	case '^': // ^ ^=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::BIT_XOR_ASG);
			goto START;
		}
		tokenEnd(Token::Category::BIT_XOR);
		goto START;
	case '%': // % %=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::MOD_ASG);
			goto START;
		}
		tokenEnd(Token::Category::MOD);
		goto START;
	case '=': // = == 
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::EQUAL);
			goto START;
		}
		tokenEnd(Token::Category::ASG);
		goto START;
	case '!': // ! !=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::NOT_EQUAL);
			goto START;
		}
		tokenEnd(Token::Category::NOT);
		goto START;
	case '*': // * *= ** ***
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::MUL_ASG);
			goto START;
		}
		else if (curr == '*')
		{
			nextChar();
			tokenEnd(Token::Category::POW);
			goto START;
		}
		tokenEnd(Token::Category::MUL);
		goto START;
	case '+': // + ++ +=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::ADD_ASG);
			goto START;
		}
		tokenEnd(Token::Category::ADD);
		goto START;
	case '-': // - -- -=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::SUB_ASG);
			goto START;
		}
		tokenEnd(Token::Category::SUB);
		goto START;
	case '&': // & && &=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::BIT_AND_ASG);
			goto START;
		}
		else if (curr == '&')
		{
			nextChar();
			tokenEnd(Token::Category::AND);
			goto START;
		}
		tokenEnd(Token::Category::BIT_AND);
		goto START;
	case '|': // | || |=
		nextChar();
		if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::BIT_OR_ASG);
			goto START;
		}
		else if (curr == '|')
		{
			nextChar();
			tokenEnd(Token::Category::OR);
			goto START;
		}
		tokenEnd(Token::Category::BIT_OR);
		goto START;
	case '<': // < << <= <<=
		nextChar();
		if (curr == '<')
		{
			nextChar();
			if (curr == '=')
			{
				nextChar();
				tokenEnd(Token::Category::SHL_ASG);
				goto START;
			}
			tokenEnd(Token::Category::SHL);
			goto START;
		}
		else if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::LES_EQUAL);
			goto START;
		}
		tokenEnd(Token::Category::LES);
		goto START;
	case '>': // > >> >>> >= >>= >>>=
		nextChar();
		if (curr == '>')
		{
			nextChar();
			if (curr == '>')
			{
				nextChar();
				if (curr == '=')
				{
					nextChar();
					tokenEnd(Token::Category::LSHR_ASG);
					goto START;
				}
				tokenEnd(Token::Category::LSHR);
				goto START;
			}
			else if (curr == '=')
			{
				nextChar();
				tokenEnd(Token::Category::ASHR_ASG);
				goto START;
			}
			tokenEnd(Token::Category::ASHR);
			goto START;
		}
		else if (curr == '=')
		{
			nextChar();
			tokenEnd(Token::Category::GRT_EQUAL);
			goto START;
		}
		tokenEnd(Token::Category::GRT);
		goto START;
	default:
		goto ILLEGAL;
	}
}

// 标点符号
PUNCTUATION: {
	switch (curr)
	{
	case ':':
		nextChar();
		tokenEnd(Token::Category::COLON);
		goto START;
	case ';':
		nextChar();
		tokenEnd(Token::Category::SEMICOLON);
		goto START;
	case ',':
		nextChar();
		tokenEnd(Token::Category::COMMA);
		goto START;
	case '.':
		nextChar();
		tokenEnd(Token::Category::DOT);
		goto START;
	case '?':
		nextChar();
		tokenEnd(Token::Category::QUESTION);
		goto START;
	case '(':
		nextChar();
		tokenEnd(Token::Category::LPAREN);
		goto START;
	case ')':
		nextChar();
		tokenEnd(Token::Category::RPAREN);
		goto START;
	case '[':
		nextChar();
		tokenEnd(Token::Category::LBRACKET);
		goto START;
	case ']':
		nextChar();
		tokenEnd(Token::Category::RBRACKET);
		goto START;
	case '{':
		nextChar();
		tokenEnd(Token::Category::LBRACE);
		goto START;
	case '}':
		nextChar();
		tokenEnd(Token::Category::RBRACE);
		goto START;
	default:
		goto ILLEGAL;
	}
}

// 单行注释
COMMENT_SINGLELINE: {
	if (curr == '\r' || curr == '\n' || curr == '\0')
	{
		goto START;
	}
	nextChar();
	goto COMMENT_SINGLELINE;
}

// 多行注释
COMMENT_MULTILINE: {
	if (curr == '*')
	{
		nextChar();
		goto COMMENT_MULTILINE_STAR;
	}
	else if (curr == '\0') // 在文件结束的时候都没有匹配到*/，说明注释符号有误
	{
		ERROR("Incomplete comment")
	}
	nextChar();
	goto COMMENT_MULTILINE;
}
COMMENT_MULTILINE_STAR: {
	if (curr == '/')
	{
		nextChar();
		goto START;
	}
	else if (curr == '\0') // 在文件结束的时候都没有匹配到*/，说明注释符号有误
	{
		ERROR("Incomplete comment")
	}
	nextChar();
	goto COMMENT_MULTILINE;
}

// Illegal token
ILLEGAL:
	ERROR("Illegal token")

// DFA成功结束
SUCCESS:
	return std::move(tokenStream);
}

// 吞字符
void Lexer::nextChar()
{
	switch (curr)
	{
	case '\t':
		currColumn += config.tabSpaces;
		break;
	case '\r':
		if (config.endlCR)
		{
			currLine += 1;
			currColumn = 1;
		}
		break;
	case '\n':
		if (!config.endlCR)
		{
			currLine += 1;
			currColumn = 1;
		}
		break;
	default:
		currColumn += 1;
	}
	currIndex += 1;
	curr = currIndex < code.size() ? code[currIndex] : '\0';
}

// 不吞当前字符，记录当前字符开始位置
void Lexer::tokenBegin()
{
	beginIndex = currIndex;
	beginLine = currLine;
	beginColumn = currColumn;
}

// 记录当前字符结束位置，并输出到Token流
void Lexer::tokenEnd(Token::Category category)
{
	auto sub = code.substr(beginIndex, currIndex - beginIndex); // 不包括当前字符
	tokenStream.emplace(Token{ category, beginLine, beginColumn, sub });
}
