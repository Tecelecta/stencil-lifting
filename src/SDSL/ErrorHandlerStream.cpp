#define SDSL_EXPORTS

#include "ErrorHandlerStream.h"

void ErrorHandlerStream::error(std::string_view message, std::optional<size_t> line, std::optional<size_t> column)
{
	errorCount += 1;
	out << "\033[31merror: " << message;
	printLineAndColumn(line, column);
	out << "\033[0m" << std::endl;
}

void ErrorHandlerStream::warning(std::string_view message, std::optional<size_t> line, std::optional<size_t> column)
{
	warningCount += 1;
	out << "\033[33mwarning: " << message;
	printLineAndColumn(line, column);
	out << "\033[0m" << std::endl;
}

void ErrorHandlerStream::info(std::string_view message, std::optional<size_t> line, std::optional<size_t> column)
{
	infoCount += 1;
	out << "\033[36minfo : " << message;
	printLineAndColumn(line, column);
	out << "\033[0m" << std::endl;
}

void ErrorHandlerStream::printLineAndColumn(const std::optional<size_t>& line, const std::optional<size_t>& column)
{
	if (line.has_value())
	{
		out << " @line = " << line.value();
	}
	if (column.has_value())
	{
		out << " @column = " << column.value();
	}
}
