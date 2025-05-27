#pragma once

#include "AST.h"
#include "ErrorHandler.h"

#include "VCG/Operation.h"

class OperationMatcher
{
protected:
	explicit OperationMatcher(ErrorHandler& errorHandler)
		: errorHandler(errorHandler) {}

	/**
		* @brief 根据构造函数的目标类型和源操作数的类型，确定该构造函数对应的操作对象
		* @param[in] context vcg上下文对象
		* @param[out] isBasicType 此类型是否为基本类型
		* @param[in] dstType 构造函数的目标类型
		* @param[in] srcType 源操作数类型
		* @param[out] opLine 行号，报错用
		* @param[out] opColumn 列号，报错用
		* @details
		* 如果不是基本类型，不进行匹配且不报错
		*/
	Operation matchBasicTypeConstructor(
		Context* context, bool& isBasicType,
		Type dstType, Type srcType,
		size_t opLine, size_t opColumn);

	/**
		* @brief 根据构造函数的目标类型和源操作数的类型，确定该构造函数对应的操作对象
		* @param[in] context vcg上下文对象
		* @param[out] isBasicType 此类型是否为基本类型
		* @param[in] dstType 构造函数的目标类型
		* @param[in] srcType 源操作数类型
		* @param[out] opLine 行号，报错用
		* @param[out] opColumn 列号，报错用
		* @details
		* 如果不是基本类型，不进行匹配且不报错
		*/
	Operation matchIntrinsicFunction(
		Token token, Context* context,
		const std::vector<Type>& srcType, const std::vector<bool>& isImm);

	/**
		* @brief 根据AST的类别和源操作数的类型，确定该运算符表达式对应的操作对象
		* @param[in] token 表达式的Token，只能是各种运算符
		* @param[in] context vcg上下文对象
		* @param[in] leftType 运算符左边的类型，如果是左单目运算符则忽略
		* @param[in] rightType 运算符右边的类型，如果是右单目运算符则忽略
		* @param[in] leftIsImm 运算符左边是否为以立即数形式表示常量
		* @param[in] rightIsImm 运算符右边是否为以立即数形式表示常量 
		* @param[out] leftCast 运算符左边是否需要隐式转换 
		* @param[out] rightCast 运算符右边是否需要隐式转换
		* @details
		* 如果不是完全匹配，则先后通过继承、协变和隐式转换他，尝试匹配。
		* 如果有一部分立即数作为源操作数，尽量向非立即数的源操作数类型转换。
		* 如果是赋值运算符，尽量向左值的类型转换。
		* 两边都是立即数被常量折叠，否则立即数转为另一边的类型
		* 隐式转换多出的算子赋给param[out]Cast EG int b + float a -> 新算子 leftCast = int2float(b) -> 返回 float.add
		* 立即数没有多出的算子 EG 1 + float a -> 1.0 = 常量折叠(1) -> 返回 float.add
		*/
	Operation matchOperatorExpression(
		Token token, Context* context,
		Type leftType, Type rightType, bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast);

private:
	// matchOperatorExpression中匹配赋值运算：=
	Operation matchAssignmentOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配算数运算：+ -
	Operation matchUnaryArithmeticOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配算数运算：+ - * / %
	Operation matchBinaryArithmeticOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配逻辑运算：&& || !
	Operation matchUnaryLogicalOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配逻辑运算：&& || !
	Operation matchBinaryLogicalOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配位运算：& | ^ ~
	Operation matchUnaryBitwiseOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配位运算：& | ^ ~
	Operation matchBinaryBitwiseOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配位运算：<< >>
	Operation matchShiftOperation(
		bool asg, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配关系运算：<= >= < > == !=
	Operation matchRelationalOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配赋值运算：+= -= *= /= %=
	Operation matchArithmeticAssignmentOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression中匹配赋值运算：&= |= ^=
	Operation matchBitwiseAssignmentOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

protected:
	ErrorHandler& errorHandler;
};
