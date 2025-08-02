#pragma once

#include "VCG/Section.h"

#include <variant>
#include <stack>

struct ValueDefinition
{
	enum class Position : size_t
	{
		CONSTANT, VARIABLE, PARAMETER, RETURN
	} position = Position::CONSTANT; 
	Value* value = nullptr;
};

struct FunctionDefinition
{
	Section* section = nullptr;
	std::vector<Value*> returnValues;
	std::unordered_map<String, size_t> closureSymbols;
};

struct StructDefinition
{
	StructType type;
	std::vector<String> elemNames;
	std::vector<Value*> initValues;
};

typedef std::variant<std::monostate, Type, ValueDefinition,
	FunctionDefinition, StructDefinition> SymbolMeaning;

struct SymbolLayerAndMeaning
{
	size_t layer = 0;
	SymbolMeaning meaning;
};

/// 
class SymbolTable final
{
public:
	void pushLayer(size_t n = 1) { layer += n; }

	void popLayer(size_t n = 1);

	void defineSymbol(String symbol, SymbolMeaning meaning);

	void updateSymbol(String symbol, SymbolMeaning meaning);

	SymbolMeaning findSymbol(String symbol);

	SymbolMeaning findSymbol(String symbol, size_t& layer);

	size_t getCurrentLayer() const { return layer; }

	std::unordered_map<String, VariableValue*> getTopVariable();

private:
	size_t layer = 0;
	std::unordered_map<String, std::stack<SymbolLayerAndMeaning>> table;
};
