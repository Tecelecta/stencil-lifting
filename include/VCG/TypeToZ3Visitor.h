#pragma once

#include "Type.h"

#include "SMT.h"

class TypeToZ3Visitor : public Type::Visitor<z3::sort>
{
public:
	explicit TypeToZ3Visitor(z3::context& z3ctx);

public:
	z3::context& z3ctx;
};

Type getTypeFromZ3(Context* context, z3::sort src);
