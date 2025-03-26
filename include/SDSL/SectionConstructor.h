#pragma once

#include "OperationMatcher.h"
#include "SymbolTable.h"
#include "VCG/Context.h"

// AST -> Section
class SectionConstructor final : private OperationMatcher
{
public:
	SDSL_API explicit SectionConstructor(ErrorHandler& errorHandler, Context* context);

	/**
		* @brief 对一个编译单元进行语义分析
		* @param[in] ast 语法树根
		* @param[in] context 计算图上下文
		* @param[out] allFunctions 在尾部添加编译成功的函数
		* @return 是否编译成功
		*/
	SDSL_API bool runOnCompileUnit(AST* ast, std::vector<FunctionDefinition>& allFunctions);

private:
	void assertTokenNode(AST* ast, Token::Category category);

	void assertTokenNode(AST* ast, Token::Category category, std::string_view text);

	bool runOnFunctionDefinition(AST* ast, FunctionDefinition& fn);

	bool runOnFunctionParameter(AST* ast, SimpleSection::Builder& sectionBuilder);

	bool runOnFunctionReturn(AST* ast, SimpleSection::Builder& sectionBuilder, std::vector<String>& returnSymbols);

	bool runOnSimpleRegion(AST* ast, SimpleSection::Builder& sectionBuilder);

	bool runOnSimpleRegionPushLayer(AST* ast, SimpleSection::Builder& sectionBuilder);

	bool runOnIfStatement(AST* ast, SimpleSection::Builder& outerBuilder);

	bool runOnDoStatement(AST* ast, SimpleSection::Builder& outerBuilder);

	bool runOnForStatement(AST* ast, SimpleSection::Builder& outerBuilder);

	bool runOnVariableDefinition(AST* ast, SimpleSection::Builder& sectionBuilder);

	bool runOnConstantDefinition(AST* ast);

	bool runOnStructDefinition(AST* ast);

	bool runOnStructBody(AST* ast, StructDefinition& def);
		
	bool runOnAssignStatement(AST* ast, SimpleSection::Builder& sectionBuilder);
		
	Value* runOnUnaryExpression(AST* ast, SimpleSection::Builder& sectionBuilder);
		
	Value* runOnBinaryExpression(AST* ast, SimpleSection::Builder& sectionBuilder);
		
	Value* runOnSingleReturnCall(AST* ast, SimpleSection::Builder& sectionBuilder);
		
	std::vector<Value*> runOnMultipleReturnCall(AST* ast, SimpleSection::Builder& sectionBuilder);
		
	Value* runOnArrayElement(AST* ast, SimpleSection::Builder& sectionBuilder,
		bool isLeft, String& symbol, ValueDefinition& valueDefinition);
		
	Value* runOnStructElement(AST* ast, SimpleSection::Builder& sectionBuilder,
		bool isLeft, String& symbol, ValueDefinition& valueDefinition);
		
	Value* runOnTupleInitializer(AST* ast, SimpleSection::Builder& sectionBuilder);

	Type getTypeObject(AST* ast);

	Value* getLeftValueOperand(AST* ast, SimpleSection::Builder& sectionBuilder, String& symbol, ValueDefinition& valueDefinition);

	Value* getRightValueOperand(AST* ast, SimpleSection::Builder& sectionBuilder);

	Value* getFixedValueOperand(AST* ast);

	std::vector<Value*> getArgumentList(AST* ast, SimpleSection::Builder& sectionBuilder);

	std::vector<Value*> getArraySubscript(AST* ast, SimpleSection::Builder& sectionBuilder);

	Value* getBeginVariable(Value* head, Value* tail, Value* step, SimpleSection::Builder& sectionBuilder);

	Value* getTimesVariable(Value* head, Value* tail, Value* step, SimpleSection::Builder& sectionBuilder);

	void checkRedefinition(String symbol, size_t line, size_t column);

	Value* checkClosureParam(VariableValue* value, SimpleSection::Builder& sectionBuilder, size_t line, size_t column);

	void checkClosureCall(Section* callee, std::vector<Value*>& args);

public:
	// bool allowRedefinition = false;
	bool allowRedefinition = true;

private:
	SymbolTable symbolTable;
	Context* context = nullptr;
	ConstantValue* defaultLoopStep = nullptr;
};
