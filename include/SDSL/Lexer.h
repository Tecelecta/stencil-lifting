#pragma once

#include "Token.h"
#include "ErrorHandler.h"

#include <queue>

/// 词法分析器类
class Lexer final
{
public:
	explicit Lexer(ErrorHandler& errorHandler) : errorHandler(errorHandler) {}
	DISABLE_COPY_MOVE(Lexer)

	SDSL_API std::queue<Token> run(std::string_view code);

private:
	void nextChar();
	void tokenBegin();
	void tokenEnd(Token::Category category);

public:
	struct Config
	{
		uint32_t tabSpaces = 4;
		bool endlCR = false; // MacOS以'\r'作为换行符号
	} config;

private:
	std::string_view code;
	std::queue<Token> tokenStream;
	ErrorHandler& errorHandler;
	size_t currIndex = 0;
	size_t currLine = 1;
	size_t currColumn = 1;
	size_t beginIndex = 0;
	size_t beginLine = 1;
	size_t beginColumn = 1;
	char curr = '\0';
};
