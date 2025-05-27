#pragma once

#include "Value.h"

#include "SMT.h"

class ConstantToZ3Visitor : public Type::Visitor<z3::expr, Symbol>
{
public:
	explicit ConstantToZ3Visitor(z3::context& z3ctx);

public:
	z3::context& z3ctx;
};
