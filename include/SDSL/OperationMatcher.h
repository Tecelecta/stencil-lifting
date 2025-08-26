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
		* @brief 
		* @param[in] context vcg
		* @param[out] isBasicType 
		* @param[in] dstType 
		* @param[in] srcType 
		* @param[out] opLine 
		* @param[out] opColumn 
		* @details
		* 
		*/
	Operation matchBasicTypeConstructor(
		Context* context, bool& isBasicType,
		Type dstType, Type srcType,
		size_t opLine, size_t opColumn);

	/**
		* @brief 
		* @param[in] context vcg
		* @param[out] isBasicType 
		* @param[in] dstType 
		* @param[in] srcType 
		* @param[out] opLine 
		* @param[out] opColumn 
		* @details
		* 
		*/
	Operation matchIntrinsicFunction(
		Token token, Context* context,
		const std::vector<Type>& srcType, const std::vector<bool>& isImm);

	/**
		* @brief AST
		* @param[in] token Token
		* @param[in] context vcg
		* @param[in] leftType 
		* @param[in] rightType 
		* @param[in] leftIsImm 
		* @param[in] rightIsImm  
		* @param[out] leftCast  
		* @param[out] rightCast 
		* @details
		* 
		* 
		* 
		* 
		* param[out]Cast EG int b + float a ->  leftCast = int2float(b) ->  float.add
		*  EG 1 + float a -> 1.0 = (1) ->  float.add
		*/
	Operation matchOperatorExpression(
		Token token, Context* context,
		Type leftType, Type rightType, bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast);

private:
	// matchOperatorExpression=
	Operation matchAssignmentOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression+ -
	Operation matchUnaryArithmeticOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression+ - * / %
	Operation matchBinaryArithmeticOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression&& || !
	Operation matchUnaryLogicalOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression&& || !
	Operation matchBinaryLogicalOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression& | ^ ~
	Operation matchUnaryBitwiseOperation(
		std::string op, Context* context,
		Type rightType,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression& | ^ ~
	Operation matchBinaryBitwiseOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression<< >>
	Operation matchShiftOperation(
		bool asg, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression<= >= < > == !=
	Operation matchRelationalOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression+= -= *= /= %=
	Operation matchArithmeticAssignmentOperation(
		std::string uop, std::string sop, std::string op,
		Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

	// matchOperatorExpression&= |= ^=
	Operation matchBitwiseAssignmentOperation(
		std::string op, Context* context,
		Type leftType, Type rightType,
		bool leftIsImm, bool rightIsImm,
		Operation& leftCast, Operation& rightCast,
		size_t opLine, size_t opColumn);

protected:
	ErrorHandler& errorHandler;
};
