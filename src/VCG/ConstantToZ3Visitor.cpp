#include "ConstantToZ3Visitor.h"
#include "Context.h"

ConstantToZ3Visitor::ConstantToZ3Visitor(z3::context& z3ctx) : z3ctx(z3ctx)
{
	defaultFunction = [this](Type type, Symbol value)
		{
			if (type.getName().equals("Integer"))
			{
				return this->z3ctx.int_val(value.cast<Integer>().getData().str().c_str());
			}
			if (type.getName().equals("Real"))
			{
				return this->z3ctx.real_val(value.cast<Decimal>().getData().str().c_str());
			}
			if (type.getName().equals("Logic"))
			{
				return this->z3ctx.bool_val(value.cast<Logic>().isTrue());
			}
			return z3::expr(this->z3ctx);
		};

	visitTemplateType = [this](TemplateType type, Symbol value)
		{
			if (type.getName().equals("int"))
			{
				assert(value.cast<BitVector>().size() <= 32);
				return this->z3ctx.bv_val(
					(uint64_t)value.cast<BitVector>().getData().to_ulong(),
					type.getArgument(0).cast<Integer>().getData().convert_to<uint32_t>());
			}
			if (type.getName().equals("float"))
			{
				// TODO
				assert(false);
			}
			return z3::expr(this->z3ctx);
		};
}
