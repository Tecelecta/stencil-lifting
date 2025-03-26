#include "SymbolTable.h"

void SymbolTable::popLayer(size_t n)
{
	layer -= n;
	for (auto& [symbol, meaningStack] : table)
	{
		while (!meaningStack.empty() && meaningStack.top().layer > layer)
		{
			meaningStack.pop();
		}
	}
}

void SymbolTable::defineSymbol(String symbol, SymbolMeaning meaning)
{
	auto& meaningStack = table[symbol];
	if (meaningStack.empty() || meaningStack.top().layer < layer)
	{
		meaningStack.push({ layer, meaning });
	}
	else
	{
		meaningStack.top().meaning = std::move(meaning);
	}
}

void SymbolTable::updateSymbol(String symbol, SymbolMeaning meaning)
{
	auto& meaningStack = table[symbol];
	assert(!meaningStack.empty());
	meaningStack.top().meaning = meaning;
}

SymbolMeaning SymbolTable::findSymbol(String symbol)
{
	auto& meaningStack = table[symbol];
	return meaningStack.empty() ? SymbolMeaning() : meaningStack.top().meaning;
}

SymbolMeaning SymbolTable::findSymbol(String symbol, size_t& layer)
{
	auto& meaningStack = table[symbol];
	if (meaningStack.empty())
	{
		return SymbolMeaning();
	}
	layer = meaningStack.top().layer;
	return meaningStack.top().meaning;
}

std::unordered_map<String, VariableValue*> SymbolTable::getTopVariable()
{
	std::unordered_map<String, VariableValue*> result;
	for (const auto& [symbol, meaningStack] : table)
	{
		if (!meaningStack.empty())
		{
			if (auto def = std::get_if<ValueDefinition>(&meaningStack.top().meaning))
			{
				if (auto variableValue = dynamic_cast<VariableValue*>(def->value))
				{
					result.emplace(symbol, variableValue);
				}
			}
		}
	}
	return result;
}
