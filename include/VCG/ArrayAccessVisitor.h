#pragma once

#include "Graph.h"
#include "OptimizeGroupPass.h"

struct AffineArrayIndex
{
	GroupNormalForm factor;
	Integer scale;
	GroupNormalForm term;
	Integer offset;

	DECL_HASH_EQ_NE_API(AffineArrayIndex)
};

struct AffineArrayAccess
{
	Value* base = nullptr;
	AffineArrayIndex index;

	DECL_HASH_EQ_NE_API(AffineArrayAccess)
};

STD_HASH(AffineArrayIndex)
STD_HASH(AffineArrayAccess)

class ArrayIndexVisitor : public Value::Visitor<void, uint32_t>
{
protected:
	VCG_API ArrayIndexVisitor();

	virtual void visitIndexParameter(InputValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexLoopTemplate(InputValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexLoopBound(BoundValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexCopyOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexSelectOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexAddOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexSubOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexNegOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexMulOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitIndexNumeral(ConstantValue* value, uint32_t dst_vertex) {}

private:
	std::unordered_map<std::string_view, void (ArrayIndexVisitor::*)(OperationValue*, uint32_t)> evaluate;
};

class ArrayAccessVisitor : public Value::Visitor<void, uint32_t>
{
protected:
	VCG_API ArrayAccessVisitor();

	virtual void visitArrayParameter(InputValue* value, uint32_t dst_vertex) {}
	virtual void visitArrayResult(ResultValue* value, uint32_t dst_vertex) {}
	virtual void visitArrayCopyOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitArraySelectOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitArrayGetOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitArraySetOp(OperationValue* value, uint32_t dst_vertex) {}
	virtual void visitArrayPhi(PhiValue* value, uint32_t dst_vertex) {}
	virtual void visitArrayConstant(ConstantValue* value, uint32_t dst_vertex) {}

private:
	std::unordered_map<std::string_view, void (ArrayAccessVisitor::*)(OperationValue*, uint32_t)> evaluate;
};
