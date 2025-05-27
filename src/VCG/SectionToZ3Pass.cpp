#include "SectionToZ3Pass.h"

inline z3::expr createUnaryOp(const std::vector<z3::expr>& src, std::function<z3::expr(z3::expr)> op)
{
	assert(src.size() == 1);
	return op(src[0]);
}

inline z3::expr createBinaryOp(const std::vector<z3::expr>& src, std::function<z3::expr(z3::expr, z3::expr)> op)
{
	assert(src.size() == 2);
	return op(src[0], src[1]);
}

inline z3::expr createReduceOp(const std::vector<z3::expr>& src, std::function<z3::expr(z3::expr, z3::expr)> op)
{
	assert(!src.empty());
	return std::reduce(src.begin() + 1, src.end(), src.front(), op);
}

SectionToZ3Pass::SectionToZ3Pass(z3::context& z3ctx)
	: z3ctx(z3ctx), typeVisitor(z3ctx), constantVisitor(z3ctx)
{
	opMap["copy"] = opMap["bitcast"] =
		[this](const std::vector<z3::expr>& src)
		{
			return src[0];
		};

	formMap["copy"] =
		[](const std::vector<Summary::Form>& src)
		{
			return src[0];
		};

	opMap["select"] =
		[this](const std::vector<z3::expr>& src)
		{
			return z3::ite(src[0], src[1], src[2]);
		};

	formMap["select"] =
		[](const std::vector<Summary::Form>& src)
		{
			if (src[0] == Summary::Form::RANGE)
			{
				if (src[1] == Summary::Form::INDEX && src[2] == Summary::Form::INDEX)
				{
					return Summary::Form::INDEX;
				}
				if (src[1] == Summary::Form::RANGE && src[2] == Summary::Form::RANGE)
				{
					return Summary::Form::RANGE;
				}
			}
			return Summary::Form::ELEM;
		};

	opMap["Integer.neg"] = opMap["Real.neg"] = opMap["int.neg"] = opMap["float.neg"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createUnaryOp(src, [](const z3::expr& src) { return -src; });
		};

	opMap["Integer.add"] = opMap["Real.add"] = opMap["int.add"] = opMap["float.add"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 + src2; });
		};

	opMap["Integer.sub"] = opMap["Real.sub"] = opMap["int.sub"] = opMap["float.sub"]
		= [this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 - src2; });
		};

	opMap["Integer.mul"] = opMap["Real.mul"] = opMap["int.mul"] = opMap["float.mul"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 * src2; });
		};

	opMap["Integer.div"] = opMap["Real.div"] = opMap["int.div"] = opMap["float.div"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 / src2; });
		};

	opMap["Integer.mod"] = opMap["int.mod"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 % src2; });
		};

	opMap["Integer.max"] = opMap["int.max"] = opMap["Real.max"] = opMap["float.max"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, z3::max);
		};

	opMap["Integer.min"] = opMap["int.min"] = opMap["Real.min"] = opMap["float.min"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, z3::min);
		};

	opMap["Integer.abs"] = opMap["int.abs"] = opMap["Real.abs"] = opMap["float.abs"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createUnaryOp(src, z3::abs);
		};

	formMap["Integer.neg"] = formMap["int.neg"] = formMap["Integer.add"] = formMap["int.add"] =
	formMap["Integer.sub"] = formMap["int.sub"] = formMap["Integer.mul"] = formMap["int.mul"] =
	formMap["Integer.max"] = formMap["int.max"] = formMap["Integer.min"] = formMap["int.min"] =
	formMap["Integer.abs"] = formMap["int.abs"] =
		[](const std::vector<Summary::Form>& src)
		{
			for (auto form : src)
			{
				if (form != Summary::Form::INDEX)
				{
					return Summary::Form::ELEM;
				}
			}
			return Summary::Form::INDEX;
		};

	opMap["bit.and"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 & src2; });
		};

	opMap["bit.xor"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 ^ src2; });
		};

	opMap["bit.or"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 | src2; });
		};

	opMap["bit.not"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createUnaryOp(src, [](const z3::expr& src) { return ~src; });
		};

	opMap["Integer.lt"] = opMap["int.lt"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 < src2; });
		};

	opMap["Integer.le"] = opMap["int.le"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 <= src2; });
		};

	opMap["Integer.gt"] = opMap["int.gt"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 > src2; });
		};

	opMap["Integer.ge"] = opMap["int.ge"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 >= src2; });
		};

	opMap["Integer.eq"] = opMap["int.eq"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 == src2; });
		};

	opMap["Integer.ne"] = opMap["int.ne"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createBinaryOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 != src2; });
		};

	formMap["Integer.lt"] = formMap["int.lt"] = formMap["Integer.le"] = formMap["int.le"] =
	formMap["Integer.gt"] = formMap["int.gt"] = formMap["Integer.ge"] = formMap["int.ge"] =
	formMap["Integer.eq"] = formMap["int.eq"] = formMap["Integer.ne"] = formMap["int.ne"] =
		[](const std::vector<Summary::Form>& src)
		{
			for (auto form : src)
			{
				if (form != Summary::Form::INDEX)
				{
					return Summary::Form::ELEM;
				}
			}
			return Summary::Form::RANGE;
		};

	opMap["Logic.and"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 && src2; });
		};

	opMap["Logic.or"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createReduceOp(src, [](const z3::expr& src1, const z3::expr& src2) { return src1 || src2; });
		};

	opMap["Logic.not"] =
		[this](const std::vector<z3::expr>& src)
		{
			return createUnaryOp(src, [](const z3::expr& src) { return !src; });
		};

	formMap["Logic.and"] = formMap["Logic.or"] = formMap["Logic.not"] =
		[](const std::vector<Summary::Form>& src)
		{
			for (auto form : src)
			{
				if (form != Summary::Form::RANGE)
				{
					return Summary::Form::ELEM;
				}
			}
			return Summary::Form::RANGE;
		};

	opMap["Real.sign"] =
		[this](const std::vector<z3::expr>& src)
		{
			auto abs0 = z3::abs(src[0]);
			return z3::ite(src[1] >= 0, abs0, -abs0);
		};

	defaultFunction = [](Value* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			return false; 
		};

	visitInputValue = [this](InputValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			assert(!callVector.empty());
			auto z3_sort = typeVisitor(value->getType());
			if (z3_sort)
			{
				assert(getOperandsByVertex(i).size() == 1);
				const auto& src_summary = summaryVector[getOperandsByVertex(i)[0]];
				auto& dst_summary = summaryVector[i];
				if (auto branch = dynamic_cast<BinaryBranch*>(callVector.back()->getCaller()))
				{
					auto cond_vertex = vertexId({ branch->getCondition(),
						std::vector<SectionCall*>(callVector.begin(), callVector.end() - 1) });
					if (summaryVector[cond_vertex].form == Summary::Form::RANGE)
					{
						z3::expr if_cond = summaryVector[cond_vertex].base;
						if (callVector.back() == branch->getElseBranch())
						{
							if_cond = simplifyUseTactic(!if_cond, true);
						}
						dst_summary = src_summary.copyBase();
						for (const auto& branch : src_summary.branches)
						{
							auto result_cond = simplifyUseTactic(branch.cond && if_cond);
							if (!result_cond.is_false())
							{
								dst_summary.mergeBranch({ result_cond, branch.elem, branch.layer, branch.index });
							}
						}
					}
					else
					{
						dst_summary = src_summary;
					}
				}
				else
				{
					dst_summary = src_summary;
				}
				return true;
			}
			return false;
		};

	visitBoundValue = [this](BoundValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			if (auto loop = dynamic_cast<ParallelLoop*>(value->getSection()))
			{
				auto index = value->getIndex();
				std::stringstream ss;
				ss << "b_" << parallelLoopBoundMap.size() + 1;
				auto bound = this->z3ctx.int_const(ss.str().c_str());
				summaryVector[i] = Summary::createScalar(bound, Summary::Form::INDEX);
				parallelLoopBoundMap.emplace(i, std::move(bound));
				return true;
			}
			return false;
		};

	visitOperationValue = [this](OperationValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			auto z3_sort = typeVisitor(value->getType());
			if (z3_sort)
			{
				std::vector<Summary> src_z3;
				std::vector<Summary::Form> src_form;
				for (auto j : getOperandsByVertex(i))
				{
					src_z3.push_back(summaryVector[j]);
					src_form.push_back(summaryVector[j].form);
				}
				if (value->getOperation().getName().equals("array.getAt"))
				{
					auto w = getArrayBound(this->z3ctx, value->getSrcVectorSize() - 1);
					summaryVector[i] = createArrayGetSummary(callVector.size(), src_z3, w, z3_sort);
					return true;
				}
				if (value->getOperation().getName().equals("array.setAt"))
				{
					auto w = getArrayBound(this->z3ctx, value->getSrcVectorSize() - 2);
					summaryVector[i] = createArraySetSummary(callVector.size(), src_z3, w);
					return true;
				}
				if (value->getOperation().getName().equals("tuple.getAt"))
				{
					auto tuple = value->getSrc(0);
					z3::expr base = summaryVector[vertexId({ tuple, callVector })].branches[0].elem;
					auto index = dynamic_cast<ConstantValue*>(value->getSrc(1))->getValue().cast<Integer>().getData().convert_to<size_t>();
					auto tuple_iter = tupleValueMap.find(std::pair<Z3_ast, size_t>{ base, index });						
					if (tuple_iter != tupleValueMap.end())
					{
						summaryVector[i] = summaryVector[tuple_iter->second];
						return true;
					}
					if (z3_sort.is_bool())
					{
						summaryVector[i] = Summary::createRange(this->z3ctx, i);
						z3::expr elem = summaryVector[i].base;
						tupleElemMap.emplace(elem, std::pair<Z3_ast, size_t>{ base, index });
						tupleValueMap.emplace(std::pair<Z3_ast, size_t>{ base, index }, i);
					}
					else
					{
						if (z3_sort.is_int())
						{
							summaryVector[i] = Summary::initScalar(z3_sort, i, Summary::Form::INDEX);
						}
						else
						{
							summaryVector[i] = Summary::initScalar(z3_sort, i, Summary::Form::ELEM);
						}
						z3::expr elem = summaryVector[i].branches[0].elem;
						tupleElemMap.emplace(elem, std::pair<Z3_ast, size_t>{ base, index });
						tupleValueMap.emplace(std::pair<Z3_ast, size_t>{ base, index }, i);
					}
					return true;
				}
				auto op_iter = opMap.find(value->getOperation().getName().view());
				if (op_iter != opMap.end())
				{
					if (z3_sort.is_array())
					{
						assert(false);
					}
					else
					{
						Summary::Form form = Summary::Form::ELEM;
						auto form_iter = formMap.find(value->getOperation().getName().view());
						if (form_iter != formMap.end())
						{
							form = form_iter->second(src_form);
						}
						if (form == Summary::Form::RANGE)
						{
							summaryVector[i] = createRangeSummary(src_z3, op_iter->second, this->z3ctx);
						}
						else
						{
							summaryVector[i] = createScalarSummary(callVector.size(), src_z3, op_iter->second, z3_sort, form);
						}
					}
					return true;
				}
				else if (!z3_sort.is_array())
				{
					auto fn = getUninterprered(value->getOperation());
					summaryVector[i] = createScalarSummary(callVector.size(), src_z3, 
						[fn](const std::vector<z3::expr>& src)
						{
							z3::expr_vector args(fn.ctx());
							for (const auto& arg : src)
							{
								args.push_back(arg);
							}
							return fn(args);
						}, z3_sort, Summary::Form::ELEM);
					return true;
				}
				std::cerr << value->getOperation().getName() << std::endl;
				assert(false); // 缺算子
			}
			return false;
		};

	visitResultValue = [this](ResultValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			if (dynamic_cast<IterateLoop*>(value->getCall()->getCallee()) != nullptr)
			{
				return false;
			}
			assert(getOperandsByVertex(i).size() == 1);
			summaryVector[i] = summaryVector[getOperandsByVertex(i)[0]];
			return true;
		};

	visitPhiValue = [this](PhiValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			if (auto branch = dynamic_cast<BinaryBranch*>(value->getSection()))
			{
				auto cond_vertex = vertexId({ branch->getCondition(), callVector });
				const auto& summary0 = summaryVector[cond_vertex];
				const auto& summary1 = summaryVector[getOperandsByVertex(i)[0]];
				const auto& summary2 = summaryVector[getOperandsByVertex(i)[1]];
				if (summary0.form == Summary::Form::RANGE)
				{
					summaryVector[i] = summary1;
					summaryVector[i].mergeBranches(summary2.branches);
				}
				else
				{
					auto z3_sort = typeVisitor(value->getType());
					if (z3_sort)
					{
						if (z3_sort.is_array())
						{
							assert((Z3_ast)summary1.base == (Z3_ast)summary2.base);
							summaryVector[i] = summary1.copyBase();
							for (const auto& branch0 : summary0.branches)
							{
								for (const auto& branch1 : summary1.branches)
								{
									for (const auto& branch2 : summary2.branches)
									{
										for (uint32_t i = 0; i < branch1.index.size(); i++)
										{
											if ((Z3_ast)branch1.index[i].func != (Z3_ast)branch2.index[i].func)
											{
												return false;
											}
										}
										auto cond = simplifyUseTactic(branch0.cond && branch1.cond && branch2.cond);
										if (!cond.is_false() && branch1.layer == branch2.layer)
										{
											auto elem = z3::ite(branch0.elem, branch1.elem, branch2.elem);
											summaryVector[i].mergeBranch({ cond, elem, branch1.layer, branch1.index });
										}
									}
								}
							}
						}
						else
						{
							std::vector<Summary> src_z3;
							src_z3.push_back(summaryVector[cond_vertex]);
							for (auto j : getOperandsByVertex(i))
							{
								src_z3.push_back(summaryVector[j]);
							}
							summaryVector[i] = createScalarSummary(callVector.size(), src_z3, opMap["select"], z3_sort, Summary::Form::ELEM);
						}
						return true;
					}
					return false;
				}
				return true;
			}
			else if (auto loop = dynamic_cast<ParallelLoop*>(value->getSection()))
			{
				assert(value->getType().cast<ArrayType>() != nullptr);
				auto& summary = summaryVector[i];
				const auto& init_summary = summaryVector[getOperandsByVertex(i)[0]];
				const auto& loop_summary = summaryVector[getOperandsByVertex(i)[1]];
				// 以init incoming为新summary的初始状态
				summary = init_summary.copyBase();
				for (const auto& branch : loop_summary.branches)
				{
					if (branch.layer > callVector.size())
					{
						summary.addBranch(branch);
					}
				}
				// 利用section.boundValue补全新summary.branches的index信息
				for (auto boundValue : loop->getBoundVector())
				{
					const auto& bound_summary = summaryVector[vertexId({ boundValue, callVector })];
					if (bound_summary.form != Summary::Form::INDEX)
					{
						continue;
					}
					z3::expr bound = bound_summary.branches[0].elem;
					auto w = getArrayBound(this->z3ctx, value->getType().getNumDims());
					z3::expr_vector src_b(this->z3ctx);
					src_b.push_back(bound);
					z3::expr_vector dst_0(this->z3ctx);
					dst_0.push_back(this->z3ctx.int_val(0));
					z3::expr_vector dst_1(this->z3ctx);
					dst_1.push_back(this->z3ctx.int_val(1));
					for (auto& branch : summary.branches)
					{
						for (uint32_t j = 0; j < branch.index.size(); j++)
						{
							z3::expr scale(this->z3ctx);
							z3::expr offset(this->z3ctx);
							bool isAffine, isConstant;
							solveAffine(branch.index[j].func, src_b, dst_0, dst_1, scale, offset, isAffine, isConstant);
							if (isAffine && !isConstant)
							{
								auto skolemFunc = ((w[j] - offset) / scale).simplify();
								branch.index[j].bound = bound;
								branch.index[j].skolem_b = skolemFunc;
								z3::expr_vector dst_sk(this->z3ctx);
								dst_sk.push_back(skolemFunc);
								branch.cond = branch.cond.substitute(src_b, dst_sk);
								branch.elem = branch.elem.substitute(src_b, dst_sk);
							}
						}
						branch.cond = simplifyUseTactic(branch.cond);
						branch.elem = branch.elem.simplify();
					}
				}
				z3::expr override_cond = this->z3ctx.bool_val(false);
				for (const auto& branch : summary.branches)
				{
					override_cond = override_cond || branch.cond;
				}
				override_cond = override_cond.simplify();
				for (const auto& branch : init_summary.branches)
				{
					auto remain_cond = simplifyUseTactic(!override_cond && branch.cond, true);
					if (!remain_cond.is_false())
					{
						summary.mergeBranch({ remain_cond, branch.elem, branch.layer, branch.index });
					}
				}
				for (auto& branch : summary.branches)
				{
					branch.layer = std::min(branch.layer, callVector.size());
				}
				return true;
			}
			return false;
		};

	visitConstantValue = [this](ConstantValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			auto z3_const = constantVisitor(value->getType(), value->getValue());
			if (!(bool)z3_const)
			{
				auto z3_sort = typeVisitor(value->getType());
				if (!(bool)z3_sort)
				{
					return false;
				}
				std::stringstream ss;
				ss << "c_" << i;
				z3_const = this->z3ctx.constant(ss.str().c_str(), z3_sort);
			}
			if (z3_const.is_int())
			{
				summaryVector[i] = Summary::createScalar(z3_const, Summary::Form::INDEX);
			}
			else if (z3_const.is_bool())
			{
				summaryVector[i] = Summary::createRange(z3_const);
			}
			else
			{
				summaryVector[i] = Summary::createScalar(z3_const, Summary::Form::ELEM);
			}
			return true;
		};

	visitInvalidValue = [this](InvalidValue* value, uint32_t i, const std::vector<SectionCall*>& callVector)
		{
			auto z3_sort = typeVisitor(value->getType());
			if (z3_sort)
			{
				if (z3_sort.is_array())
				{
					summaryVector[i] = Summary::initArrayBase(z3_sort, i);
					invalidValueMap.emplace(i, summaryVector[i].base);
				}
				else
				{
					summaryVector[i] = Summary::initScalar(z3_sort, i, Summary::Form::ELEM);
					invalidValueMap.emplace(i, summaryVector[i].branches[0].elem);
				}
			}
			return true;
		};
}

bool SectionToZ3Pass::run()
{
	summaryVector = decltype(summaryVector)(getVertexNum(), Summary(z3ctx));
	return iterateSCC(*this);
}

bool SectionToZ3Pass::update(uint32_t i)
{
	if (summaryVector[i].form == Summary::Form::NONE)
	{
		const auto& path = vertexAt(i);
#ifdef _DEBUG
		std::cout << i << ": " << path.value->getName() << std::endl;
#endif // _DEBUG
		if (!SectionToZ3Pass::operator()(path.value, i, path.callVector))
		{
#ifdef _DEBUG
			std::cerr << "ERROR!!!" << std::endl;
#endif // _DEBUG
			return false;
		}
#ifdef _DEBUG
		std::cout << summaryVector[i].str() << std::endl;
#endif // _DEBUG
	}
	return true;
}

bool SectionToZ3Pass::update(const vertex_list& initUpdateList)
{
	vertexFilter = visitReachableSubGraph(getDependencyVector(), initUpdateList);
	return iterateSCC(*this);
}

void SectionToZ3Pass::invalidate(const vertex_list& arrayBaseList)
{
	auto toInvalidate = packReachableSubGraph(getDataflowVector(), arrayBaseList);
	for (auto i : toInvalidate)
	{
		summaryVector[i] = Summary(z3ctx);
	}
}

z3::expr SectionToZ3Pass::getLoopBoundCond(const z3::expr& bound, const z3::expr& begin, const z3::expr& times, const z3::expr& step) const
{
	return (begin <= bound && bound <= begin + (times - 1) * step && (bound - begin) % step == 0).simplify();
}

bool SectionToZ3Pass::runOnTrivial(uint32_t i)
{
	if (vertexFilter.empty() || vertexFilter.at(i))
	{
		return update(i);
	}
	return true;
}

z3::func_decl SectionToZ3Pass::getUninterprered(Operation op)
{
	auto iter = z3FuncMap.find(op);
	if (iter == z3FuncMap.end())
	{
		z3::sort_vector domain(z3ctx);
		for (auto type : op.getParameterTypeVector())
		{
			domain.push_back(typeVisitor(type));
		}
		auto fn = z3::function(op.getName().c_str(), domain, typeVisitor(op.getReturnType()));
		iter = z3FuncMap.emplace(op, fn).first;
	}
	return iter->second;
}
