#define VCG_EXPORTS

#include "ArrayAccessVisitor.h"
#include "ContainerHash.h"

#include <boost/pfr.hpp>

BOOST_PFR_HASH_EQ_NE_API(AffineArrayIndex)
BOOST_PFR_HASH_EQ_NE_API(AffineArrayAccess)

ArrayIndexVisitor::ArrayIndexVisitor()
{
	defaultFunction = [](Value* value, uint32_t dst_vertex) {};

	visitInputValue = [this](InputValue* value, uint32_t dst_vertex)
		{
			if (value->getType().getName().equals("Integer"))
			{
				if (value->isTemplate())
				{
					if (dynamic_cast<IterateLoop*>(value->getSection()) != nullptr)
					{
						visitIndexLoopTemplate(value, dst_vertex);
					}
				}
				else
				{
					visitIndexParameter(value, dst_vertex);
				}
			}
		};

	visitBoundValue = [this](BoundValue* value, uint32_t dst_vertex)
		{
			if (value->getType().getName().equals("Integer"))
			{
				if (dynamic_cast<IterateLoop*>(value->getSection()) != nullptr)
				{
					visitIndexLoopBound(value, dst_vertex);
				}
			}
		};

	visitOperationValue = [this](OperationValue* value, uint32_t dst_vertex)
		{
			if (value->getType().getName().equals("Integer"))
			{
				auto op = value->getOperation();
				auto func_iter = evaluate.find(op.getName().view());
				if (func_iter != evaluate.end())
				{
					// 
					(this->*func_iter->second)(value, dst_vertex);
				}
			}
		};

	visitConstantValue = [this](ConstantValue* value, uint32_t dst_vertex)
		{
			if (value->getType().getName().equals("Integer"))
			{
				visitIndexNumeral(value, dst_vertex);
			}
		};

	evaluate["copy"] = &ArrayIndexVisitor::visitIndexCopyOp;
	evaluate["select"] = &ArrayIndexVisitor::visitIndexSelectOp;
	evaluate["Integer.add"] = &ArrayIndexVisitor::visitIndexAddOp;
	evaluate["Integer.sub"] = &ArrayIndexVisitor::visitIndexSubOp;
	evaluate["Integer.neg"] = &ArrayIndexVisitor::visitIndexNegOp;
	evaluate["Integer.mul"] = &ArrayIndexVisitor::visitIndexMulOp;
}

ArrayAccessVisitor::ArrayAccessVisitor()
{
	defaultFunction = [](Value* value, uint32_t dst_vertex) {};

	visitInputValue = [this](InputValue* value, uint32_t dst_vertex)
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				if (!value->isTemplate())
				{
					visitArrayParameter(value, dst_vertex);
				}
			}
		};

	visitOperationValue = [this](OperationValue* value, uint32_t dst_vertex)
		{
			if (value->getType().cast<ArrayType>() != nullptr ||
				value->getSrc(0)->getType().cast<ArrayType>() != nullptr)
			{
				auto op = value->getOperation();
				auto func_iter = evaluate.find(op.getName().view());
				if (func_iter != evaluate.end())
				{
					// 
					(this->*func_iter->second)(value, dst_vertex);
				}
			}
		};

	visitResultValue = [this](ResultValue* value, uint32_t dst_vertex)
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				visitArrayResult(value, dst_vertex);
			}
		};

	visitPhiValue = [this](PhiValue* value, uint32_t dst_vertex)
		{
			if (value->getType().cast<ArrayType>() != nullptr)
			{
				visitArrayPhi(value, dst_vertex);
			}
		};

	visitConstantValue = [this](ConstantValue* value, uint32_t dst_vertex)
		{
			visitArrayConstant(value, dst_vertex);
		};

	evaluate["copy"] = &ArrayAccessVisitor::visitArrayCopyOp;
	evaluate["select"] = &ArrayAccessVisitor::visitArraySelectOp;
	evaluate["array.getAt"] = &ArrayAccessVisitor::visitArrayGetOp;
	evaluate["array.setAt"] = &ArrayAccessVisitor::visitArraySetOp;
}
