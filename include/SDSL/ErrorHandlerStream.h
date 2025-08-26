#pragma once

#include "ErrorHandler.h"

#include <iostream>

/// std::ostream
class ErrorHandlerStream final : public ErrorHandler
{
public:
	explicit ErrorHandlerStream(std::ostream& out = std::cerr) : out(out) {}

	SDSL_API void error(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) override;
	SDSL_API void warning(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) override;
	SDSL_API void info(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) override;

private:
	void printLineAndColumn(const std::optional<size_t>& line, const std::optional<size_t>& column);

private:
	std::ostream& out;
};
