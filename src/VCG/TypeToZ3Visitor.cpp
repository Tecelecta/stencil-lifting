#include "TypeToZ3Visitor.h"
#include "Context.h"

TypeToZ3Visitor::TypeToZ3Visitor(z3::context& z3ctx) : z3ctx(z3ctx)
{
	defaultFunction = [this](Type type)
		{
			if (type.getName().equals("Integer"))
			{
				return this->z3ctx.int_sort();
			}
			if (type.getName().equals("Real"))
			{
				return this->z3ctx.real_sort();
			}
			if (type.getName().equals("Logic"))
			{
				return this->z3ctx.bool_sort();
			}
			return z3::sort(this->z3ctx);
		};

	visitTemplateType = [this](TemplateType type)
		{
			if (type.getName().equals("int"))
			{
				return this->z3ctx.bv_sort(
					type.getArgument(0).cast<Integer>().getData().convert_to<uint32_t>());
			}
			if (type.getName().equals("float"))
			{
				return this->z3ctx.fpa_sort(
					type.getArgument(0).cast<Integer>().getData().convert_to<uint32_t>(),
					type.getArgument(1).cast<Integer>().getData().convert_to<uint32_t>());
			}
			return z3::sort(this->z3ctx);
		};

	visitArrayType = [this](ArrayType type)
		{
			z3::sort_vector sv(this->z3ctx);
			for (size_t i = 0; i < type.getNumDims(); i++)
			{
				sv.push_back(this->z3ctx.int_sort());
			}
			return this->z3ctx.array_sort(sv, (*this)(type.getElementType()));
		};
		
	visitTupleType = [this](TupleType type)
		{
			return this->z3ctx.uninterpreted_sort(type.getName().c_str());
		};
}

Type getTypeFromZ3(Context* context, z3::sort src)
{
	switch (src.sort_kind())
	{
	case Z3_BOOL_SORT:
		return context->getLogicType();
	case Z3_INT_SORT:
		return context->getIntegerType();
	case Z3_REAL_SORT:
		return context->getRealType();
	default:
		assert(false);
		return Type();
	}
}
