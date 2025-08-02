#pragma once

#include "SDSL_global.h"
#include "Constructor.h"

#include <string>
#include <optional>

/// 
class ErrorHandler
{
protected:
	ErrorHandler() = default;

public:
	DISABLE_COPY_MOVE(ErrorHandler)

	virtual void error(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) = 0;
	virtual void warning(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) = 0;
	virtual void info(std::string_view message, std::optional<size_t> line = std::nullopt, std::optional<size_t> column = std::nullopt) = 0;

public:
	size_t errorCount = 0;
	size_t warningCount = 0;
	size_t infoCount = 0;
};
