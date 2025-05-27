#pragma once

#include "z3++.h"

bool proveTrue(z3::expr a);
bool proveFalse(z3::expr a);
bool proveZero(z3::expr a);
bool proveNotZero(z3::expr a);
bool proveEquals(z3::expr a, z3::expr b);
bool proveNotEquals(z3::expr a, z3::expr b);

bool isFunctionOf(z3::expr parent, z3::expr child);

z3::expr_vector createZ3Vector(z3::context& z3ctx, const std::vector<z3::expr>& vec);

z3::expr simplifyUseTactic(z3::expr src, bool elim_and = false);

void solveAffine(z3::expr y, z3::expr_vector src_x, z3::expr_vector dst_0, z3::expr_vector dst_1,
	z3::expr& scale, z3::expr& offset, bool& isAffine, bool& isConstant);
