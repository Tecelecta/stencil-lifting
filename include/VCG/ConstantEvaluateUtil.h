#pragma once

#include "Value.h"

struct ConstantEvaluateUtil
{
	ConstantEvaluateUtil();

	Value* tryEvaluate(Context* context, Operation op, String name, const std::vector<ConstantValue*>& src);

	static size_t getSelectedOperand(ConstantValue* selector);

	std::unordered_map<std::string_view, std::function<ConstantValue* (Operation, const std::vector<ConstantValue*>&, String)>> evaluate;
};
