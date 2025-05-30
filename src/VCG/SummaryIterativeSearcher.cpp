#include "SummaryIterativeSearcher.h"
#include "LoopParallelizePass.h"
#include "ArraySetSolver.h"

#define VIZ(ID, V, LIST, SUM) \
{\
std::cout << ID << ": " << V->getName() << " <- ";\
for (auto dep : LIST)\
{\
	std::cout << dep << " ";\
}\
std::cout << std::endl;\
std::cout << SUM.str() << std::endl;\
}

SummaryIterativeSearcher::SummaryIterativeSearcher()
    : z3Pass(z3ctx), parallelizePass(z3ctx), t(z3ctx.int_const("t")), x(z3ctx.int_const("x")),
    t_begin(z3ctx.int_const("tb")), t_times(z3ctx.int_const("tm")), t_step(z3ctx.int_const("step")),
    v_t(z3ctx), v_x(z3ctx), dst_t(z3ctx)
{
    defaultFunction = [](Value* value, uint32_t i)
        {
            return true;
        };

    visitInputValue = [this](InputValue* value, uint32_t i)
        {
            if (value->getSection() == loop)
            {
                auto z3_sort = z3Pass.typeVisitor(value->getType());
                if (z3_sort)
                {
                    auto v = parallelizePass.vertexId(value);
                    if (value->isTemplate())
                    {
                        switch (value->getIndex())
                        {
                        case 0:
                            parallelizePass.summaryVector[v] = Summary::createScalar(t_begin, Summary::Form::INDEX);
                            break;
                        case 1:
                            parallelizePass.summaryVector[v] = Summary::createScalar(t_times, Summary::Form::INDEX);
                            break;
                        case 2:
                            parallelizePass.summaryVector[v] = Summary::createScalar(t_step, Summary::Form::INDEX);
                            break; 
                        default:
                            return false;
                        }
                    }
                    else
                    {
                        if (z3_sort.is_array())
                        {
                            parallelizePass.summaryVector[v] = Summary::initArrayBase(z3_sort, v);
                        }
                        else if (z3_sort.is_int())
                        {
                            parallelizePass.summaryVector[v] = Summary::initScalar(z3_sort, v, Summary::Form::INDEX);
                        }
                        else if (z3_sort.is_bool())
                        {
                            parallelizePass.summaryVector[v] = Summary::createRange(z3ctx, i);
                        }
                        else
                        {
                            parallelizePass.summaryVector[v] = Summary::initScalar(z3_sort, v, Summary::Form::ELEM);
                        }
                    }
#ifdef _DEBUG
                    VIZ(v, value, parallelizePass.getDependencyByVertex(v), parallelizePass.summaryVector[v])
#endif
                    return true;
                }
                return false;
            }
            else
            {
                auto& dst = z3Pass.summaryVector[z3Pass.vertexId({ value, {} })];
                if (dst.form == Summary::Form::NONE)
                {
                    auto v = parallelizePass.vertexId(loop->getSectionCall(0)->getArgument(value->getIndex()));
                    dst = parallelizePass.summaryVector[v];
#ifdef _DEBUG
                    std::cout << z3Pass.vertexId({ value, {} }) << ": " << value->getName() << std::endl;
                    std::cout << dst.str() << std::endl;
#endif
                }
                return true;
            }
        };

    visitBoundValue = [this](BoundValue* value, uint32_t i)
        {
            if (value->getSection() == loop)
            {
                auto counter = (t_step * t + t_begin).simplify();
                auto v = parallelizePass.vertexId(loop->getBound(0));
                parallelizePass.summaryVector[v] = Summary::createScalar(0 <= t && t <= t_times - 1, counter, Summary::Form::INDEX);
            }
            return true;
        };

    visitPhiValue = [this](PhiValue* value, uint32_t i)
        {
            if (value->getSection() == loop)
            {
                auto v0 = parallelizePass.vertexId(value->getIncoming(0));
                const auto& s0 = parallelizePass.summaryVector[v0];
                auto v1 = parallelizePass.vertexId(value->getIncoming(1));
                const auto& s1 = parallelizePass.summaryVector[v1];
                auto& summary = parallelizePass.summaryVector[parallelizePass.vertexId(value)];
                auto s2 = nextTimeStep(s1);
                summary = s2.copyBase();
                for (const auto& branch : s0.branches)
                {
                    auto cond = t == 0 && branch.cond;
                    summary.mergeBranch({ cond, branch.elem, 0, branch.index });
                }
                for (const auto& branch : s2.branches)
                {
                    auto cond = 1 <= t && t <= t_times - 1 && branch.cond;
                    summary.mergeBranch({ cond, branch.elem, 0, branch.index });
                }
#ifdef _DEBUG
                std::cout << i << ": " << value->getName() << std::endl;
                std::cout << summary.str() << std::endl;
#endif
            }
            return true;
        };

    visitResultValue = [this](ResultValue* value, uint32_t i)
        {
            if (value->getSection() == loop)
            {
                auto v = z3Pass.vertexId({ value->getSrc(), {} });
                if (!z3Pass.update(vertex_list{ v }))
                {
                    return false;
                }
                parallelizePass.summaryVector[parallelizePass.vertexId(value)] = z3Pass.summaryVector[v];
            }
            return true;
        };

    v_t.push_back(t);
    dst_t.push_back(t - 1);
    v_x.push_back(x);
}

void SummaryIterativeSearcher::runOnBody()
{
    CopySectionShallowPass subPass;
    subPass.parent = parent;
    subPass.load({ mapSectionByParent(body) });
    subPass.run();
    updateResultVector(subPass);

    z3Pass.load({ mapSectionByParent(body) });
    z3Pass.summaryVector = decltype(z3Pass.summaryVector)(z3Pass.getVertexNum(), Summary(z3ctx));
}

void SummaryIterativeSearcher::runOnLoop()
{
    CopySectionShallowPass subPass;
    subPass.parent = parent;
    subPass.load({ loop });
    subPass.run();
    updateResultVector(subPass);

    parallelizePass.load({ loop });
    parallelizePass.summaryVector = decltype(parallelizePass.summaryVector)(parallelizePass.getVertexNum(), Summary(z3ctx));

    if (constantSpecializer[0] != nullptr)
    {
        t_begin = z3ctx.int_val(constantSpecializer[0].cast<Integer>().getData().str().c_str());
    }
    if (constantSpecializer[1] != nullptr)
    {
        t_times = z3ctx.int_val(constantSpecializer[1].cast<Integer>().getData().str().c_str());
    }
    if (constantSpecializer[2] != nullptr)
    {
        t_step = z3ctx.int_val(constantSpecializer[2].cast<Integer>().getData().str().c_str());
    }

    if (iterateSCC(*this))
    {
        for (const auto& item : z3Pass.invalidValueMap)
        {
            parallelizePass.outerValueMap.emplace(item.second, z3Pass.vertexAt(item.first).value);
        }
        parallelizePass.tupleElemMap = std::move(z3Pass.tupleElemMap);
        parallelizePass.run();
        parallelizePass.sectionResultVector[0]->validate();
        updateResultVector(parallelizePass);
    }
    else
    {
        std::cerr << "Find summary failed!" << std::endl;
    }
}

bool SummaryIterativeSearcher::runOnTrivial(uint32_t i)
{
    return SummaryIterativeSearcher::operator()(vertexAt(i), i);
}

bool SummaryIterativeSearcher::runOnTypical(const vertex_list& scc)
{
#ifdef _DEBUG
    Value::Visitor<void, nullptr_t> valv;
    valv.visitOperationValue = [this](OperationValue* v, nullptr_t) 
        {
            std::cout << "{" << v->getOperation().getName() << "(";
            for (auto operand : v->getDependencyVector()) {
                std::cout << vertexId(operand) 
                    << (operand == v->getDependencyVector().back() ? ")}" : ", ");
            }
        };
    valv.visitPhiValue = [this](PhiValue* v, nullptr_t) 
        { 
            std::cout << "{Phi(" << vertexId(v->getIncoming(0)) << ", "
                << vertexId(v->getIncoming(1)) << ")}";
        };
    valv.visitInputValue = [this](InputValue* v, nullptr_t) 
        { 
            auto& calls = loop->getSectionCallVector();
            auto arg = calls[0]->getArgument(v->getIndex());
            std::cout << "{Input(" << vertexId(arg) << ")}"; 
        };
    valv.visitResultValue = [this] (ResultValue* v, nullptr_t) 
        { 
            if (vertexExists(v->getSrc()))
                std::cout << "{Result(" << vertexId(v->getSrc()) << ")}";
            else {
                std::cout << "{Result(";
                std::cout << v->getSrc();
                std::cout << "(Extern))}";
            }
        };

    std::cout << "SCC begin" << std::endl;
    for (auto i : scc)
    {
        auto valptr = vertexVector[i];
        std::cout << i << "@" << (dynamic_cast<VariableValue*>(valptr)->getSection() == loop ? "loop" : "body") 
            << " : " << valptr->getName();
        std::cout << "(" << valptr->getType().toString() << ")";
        valv(valptr, nullptr);
        std::cout << std::endl;
    }
    std::cout << "SCC end" << std::endl;
#endif // _DEBUG

    std::vector<PhiValue*> phiValueVector;
    std::unordered_map<PhiValue*, size_t> phiValueMap;
    for (uint32_t i : scc)
    {
        if (auto phiValue = dynamic_cast<PhiValue*>(vertexAt(i)))
        {
            if (phiValue->getType().cast<ArrayType>() != nullptr)
            {
                if (phiValue->getSection() == loop)
                {
                    phiValueMap.emplace(phiValue, phiValueVector.size());
                    phiValueVector.push_back(phiValue);
                }
            }
            else
            {
                std::cerr << phiValue->getName() << std::endl;
                return false;
            }
        }
    }

    vertex_list sccInputList(phiValueVector.size(), ~0);
    auto callBody = loop->getSectionCall(0);
    for (size_t i = 0; i < callBody->getArgumentVectorSize(); i++)
    {
        auto iter = std::find(phiValueVector.begin(), phiValueVector.end(), callBody->getArgument(i));
        if (iter != phiValueVector.end())
        {
            sccInputList[phiValueMap.at(*iter)] = z3Pass.vertexId({ body->getParameter(i), {} });
        }
    }

    vertex_list sccOutputList(phiValueVector.size(), ~0);
    for (size_t index = 0; index < phiValueVector.size(); index++)
    {
        auto phiValue = phiValueVector[index];
        auto resultValue = dynamic_cast<ResultValue*>(phiValue->getIncoming(1));
        if (resultValue == nullptr || resultValue->getType().cast<ArrayType>() == nullptr)
        {
            return false;
        }
        sccOutputList[index] = z3Pass.vertexId({ resultValue->getSrc(), {} });
        auto v = parallelizePass.vertexId(phiValueVector[index]->getIncoming(0));
        z3Pass.summaryVector[sccInputList[index]] = parallelizePass.summaryVector[v];
#ifdef _DEBUG
        std::cout << "SCC Input vid <- summary: " << sccInputList[index] 
            << " - loop:"<< v << ": " << parallelizePass.summaryVector[v].str();
#endif
    }
#ifdef _DEBUG
    std::cout << "SCC input vertex ids: ";
    for (auto i : sccInputList) {
        std::cout << i << " ";
    }
    for (auto i : sccOutputList) {
        std::cout << i << " ";
    }
    std::cout << std::endl;
#endif
    if (!z3Pass.update(sccOutputList))
    {
        return false;
    }

    std::vector<Summary> firstLoopVC;
    std::vector<Summary> sccOutputSummary;
    std::unordered_set<int> sccBypassList;
    std::vector<Summary>  sccBypassSummary;
    int scc_count = 0;
    for (auto sccOutput : sccOutputList)
    {
        firstLoopVC.push_back(z3Pass.summaryVector[sccOutput]);
        ArraySetSolver solver(z3ctx, t_times,
            z3Pass.vertexAt(sccOutput).value->getType().getNumDims(), z3Pass.summaryVector[sccOutput]);

        //if (solver.tryBypassTiling())
        if (false)
        { 
#ifdef _DEBUG
            std::cout << "Tiling detected\n";
#endif // _DEBUG
            sccBypassList.insert(scc_count);
        }
        else
        {
            solver.solve();
        }
#ifdef _DEBUG
		std::cout << sccOutput << ": " << z3Pass.vertexAt(sccOutput).value->getName() << std::endl;
		std::cout << solver.summary.str() << std::endl;
#endif // _DEBUG
		sccOutputSummary.emplace_back(std::move(solver.summary));
        scc_count++;
    }

    // 迭代搜索
    bool success = false;
    for (size_t step = 1; step <= 100; step++)
    {
#ifdef _DEBUG
        std::cout << "Iterate step " << step << " written values:" << std::endl;
#endif
        for (size_t index = 0; index < phiValueVector.size(); index++)
        {
            if (sccBypassList.find(index) != sccBypassList.end()) continue;

            auto v = sccInputList[index];
            auto ast = (Z3_ast)z3Pass.summaryVector[v].base;
            auto phi_summary = nextTimeStep(sccOutputSummary[index]);
            for (auto& branch : phi_summary.branches)
            {
                z3::expr_vector dst_sk(z3ctx);
                dst_sk.push_back(branch.skolem_x.value());
                branch.cond = simplifyUseTactic(branch.cond && t <= t_times - 1 && x <= t_times - 1);
            }
            summaryMap.emplace(ast, phi_summary);
#ifdef _DEBUG
            std::cout << sccOutputList[index] << ": " << z3Pass.vertexAt(sccOutputList[index]).value->getName() << std::endl;
#endif
        }

        bool converged = true;
        std::vector<Summary> sccUpdatedSummary;
        //for (const auto& original : sccOutputSummary)
        for (int index = 0; index < sccOutputSummary.size(); index++)
        {
            const auto& original = sccOutputSummary[index];
            if (sccBypassList.find(index) != sccBypassList.end())
            {
                sccUpdatedSummary.emplace_back(original);
                converged &= true;
            }
            else 
            {
                sccUpdatedSummary.emplace_back(updateSummary(original, summaryMap, converged));
            }
#ifdef _DEBUG
            auto v = sccOutputList[sccUpdatedSummary.size() - 1];
            std::cout << v << ": " << z3Pass.vertexAt(v).value->getName() << std::endl;
            std::cout << sccUpdatedSummary.back().str() << std::endl;
#endif // _DEBUG
        }

        summaryMap.clear();
        if (converged)
        {
            //std::cout << "converged!" << std::endl;
            success = true;
            break;
        }
        if (step == 100)
        {
            std::cout << "time out!" << std::endl;
            success = false;
        }
        bool diverged = false;
        for (size_t i = 0; i < sccOutputSummary.size(); i++)
        {
            sccOutputSummary[i] = std::move(sccUpdatedSummary[i]);
            if (sccOutputSummary[i].branches.size() >= 100)
            {
                diverged = true;
            }
        }
        if (diverged)
        {
            std::cout << "diverged!" << std::endl;
            success = false;
            break;
        }
    }

    for (size_t index = 0; index < sccOutputSummary.size(); index++)
    {
        if (sccBypassList.find(index) != sccBypassList.end()) continue;
        sccOutputSummary[index] = propagateSkolemX(sccOutputSummary[index]);
    }
    std::vector<Summary> loopInvarVC = sccOutputSummary;
    for (size_t index = 0; index < sccOutputSummary.size(); index++)
    {
        if (sccBypassList.find(index) != sccBypassList.end()) continue;
        loopInvarVC[index] = propagateRead(loopInvarVC[index]);
        sccOutputSummary[index] = propagateRead(nextTimeStep(sccOutputSummary[index]));
    }
        
    z3Pass.invalidate(sccInputList);
    for (size_t index = 0; index < phiValueVector.size(); index++)
    {
        auto& summary = z3Pass.summaryVector[sccInputList[index]];
        summary = sccOutputSummary[index];
        if (sccBypassList.find(index) != sccBypassList.end()) continue;
		for (auto& branch : summary.branches)
        {
			branch.cond = simplifyUseTactic(branch.cond && t <= t_times - 1);
		}

    }
    vertex_list tmpOutputList({});
    for (int i = 0; i < sccOutputList.size(); i++)
    {
        if (sccBypassList.find(i) == sccBypassList.end())
            tmpOutputList.push_back(sccOutputList[i]);
    }
    z3Pass.update(tmpOutputList);

    for (size_t index = 0; index < sccOutputList.size(); index++)
    {
        if (sccBypassList.find(index) != sccBypassList.end()) continue;

        Summary s0 = firstLoopVC[index];
        for (auto& branch : s0.branches)
        {
            auto w = getArrayBound(z3ctx, branch.index.size());
            branch.cond = branch.cond.substitute(w, branch.createIndexVector()).simplify();
            branch.elem = branch.elem.substitute(w, branch.createIndexVector()).simplify();
        }
        //s0 = propagateSkolemB(s0);
        Summary s1 = loopInvarVC[index];
        for (auto& branch : s1.branches)
        {
            auto w = getArrayBound(z3ctx, branch.index.size());
            branch.cond = branch.cond.substitute(w, branch.createIndexVector()).simplify();
            branch.elem = branch.elem.substitute(w, branch.createIndexVector()).simplify();
        }
        //s1 = propagateSkolemB(s1);
        Summary s2 = (z3Pass.summaryVector[sccOutputList[index]]);
        for (auto& branch : s2.branches)
        {
            auto w = getArrayBound(z3ctx, branch.index.size());
            branch.cond = branch.cond.substitute(w, branch.createIndexVector()).simplify();
            branch.cond = simplifyUseTactic(branch.cond && 0 <= t && t <= t_times - 1);
            branch.elem = branch.elem.substitute(w, branch.createIndexVector()).simplify();
        }
        //s2 = propagateSkolemB(s2);
        z3::expr pre = s0.iteStoreForm();
        z3::expr invar1 = s1.iteStoreForm();
        z3::expr invar2 = s2.iteStoreForm();
        if (!proveTrue(z3::implies(t == 0, pre == invar2)))
        {
            std::cerr << "Verify failed!" << std::endl;
            std::cerr << "name = \n" << z3Pass.vertexAt(sccOutputList[index]).value->getName() << std::endl;
            std::cerr << "pre = \n" << pre << std::endl;
            std::cerr << "invar1 = \n" << invar1 << std::endl;
            //throw std::logic_error("Verify failed!");
            return false;
        }
        if (!proveEquals(invar1, invar2))
        {
            std::cerr << "Verify failed!" << std::endl;
            std::cerr << "name = \n" << z3Pass.vertexAt(sccOutputList[index]).value->getName() << std::endl;
            std::cerr << "invar1 = \n" << s1.str() << std::endl;
            std::cerr << "invar2 = \n" << s2.str() << std::endl;
            //throw std::logic_error("Verify failed!");
            return false;
        }
        //std::cout << "Verify passed!" << std::endl;
    }

    for (size_t index = 0; index < sccOutputSummary.size(); index++)
    {
        auto u = parallelizePass.vertexId(phiValueVector[index]);
        if (sccBypassList.find(index) != sccBypassList.end())
        {
            parallelizePass.summaryVector[u] = propagateSkolemB(sccOutputSummary[index]);
        }
        else
        {
            parallelizePass.summaryVector[u] = getPostCond(propagateSkolemB(sccOutputSummary[index]));
        }
#ifdef _DEBUG
        auto v = sccOutputList[index];
        std::cout << v << ": " << z3Pass.vertexAt(v).value->getName() << std::endl;
        std::cout << parallelizePass.summaryVector[u].str() << std::endl;
#endif // _DEBUG
        assert(parallelizePass.summaryVector[u].checkDistinct());
    }
    return success;
}

Summary SummaryIterativeSearcher::updateSummary(const Summary& original, std::unordered_map<Z3_ast, Summary>& summaryMap, bool& converged)
{
    auto result = original.copyBase();
    for (const auto& branch : original.branches)
    {
        z3::expr_vector tmp_vec(z3ctx);
        z3::expr_vector dst_vec(z3ctx);
        converged = doSelectSubstitute(result.branches, branch) && converged;
    }
    return result;
}

bool SummaryIterativeSearcher::doSelectSubstitute(std::vector<Summary::Branch>& dst_value, const Summary::Branch& src_branch)
{
    std::unordered_set<Z3_ast> visited;
    auto actions = getSelectSubstitute(src_branch.cond, src_branch.elem, x, visited);
    if (actions.empty())
    {
        dst_value.push_back(src_branch);
        return true;
    }
    for (const auto& action : actions)
    {
        z3::expr_vector src(z3ctx);
        src.push_back(action.src);
        z3::expr_vector dst(z3ctx);
        dst.push_back(action.dst);
        Summary::Branch dst_branch = { action.cond, src_branch.elem, src_branch.layer, src_branch.index, src_branch.skolem_x };
        dst_branch.elem = dst_branch.elem.substitute(src, dst).simplify();
        dst_value.emplace_back(std::move(dst_branch));
    }
    return false;
}

std::vector<SummaryIterativeSearcher::SelectSubstituteAction> SummaryIterativeSearcher::getSelectSubstitute(
    z3::expr cond, z3::expr elem, z3::expr r, std::unordered_set<Z3_ast>& visited)
{
    if (visited.count(elem) > 0 || cond.is_false())
    {
        return {};
    }
    assert(!elem.is_array());
    if (elem.decl().decl_kind() == Z3_OP_SELECT)
    {
        auto arr = elem.arg(0);
        auto iter = summaryMap.find((Z3_ast)arr);
        if (iter != summaryMap.end())
        {
            std::vector<SelectSubstituteAction> result;
            z3::expr_vector read_index(z3ctx);
            for (uint32_t i = 1; i < elem.num_args(); i++)
            {
                read_index.push_back(elem.arg(i));
            }
            auto w = getArrayBound(z3ctx, read_index.size());
            const auto& arr = iter->second;
            auto no_read_cond = z3ctx.bool_val(true);
            for (const auto& branch : arr.branches)
            {
                z3::expr skolem = branch.skolem_x.value();
                skolem = skolem.substitute(w, read_index).simplify();
                z3::expr_vector dst_sk(z3ctx);
                dst_sk.push_back(skolem);
                z3::expr read_cond = branch.cond;
                read_cond = read_cond.substitute(v_x, dst_sk).simplify();

                z3::expr_vector dst_x(z3ctx);
                dst_x.push_back(r);
                read_cond = read_cond.substitute(v_t, dst_x);
                read_cond = read_cond.substitute(w, read_index);
#ifdef _DEBUG
                std::cout << "read cond:\n" << (read_cond && cond);
#endif
                read_cond = simplifyUseTactic(cond && read_cond);
#ifdef _DEBUG
                std::cout << " | simplify:\n" << read_cond << std::endl;
#endif
                no_read_cond = no_read_cond && !read_cond;

                if (!read_cond.is_false())
                {
                    z3::expr next_elem = branch.elem;
                    next_elem = next_elem.substitute(v_x, dst_sk).simplify();
                    z3::sort_vector domain(z3ctx);
                    domain.push_back(next_elem.get_sort());
                    domain.push_back(z3ctx.int_sort());
                    auto fn = z3ctx.function("read", domain, next_elem.get_sort());
                    next_elem = fn(next_elem, skolem);
#ifdef _DEBUG
                    std::cout << "subtitue src: " << elem << std::endl;
                    std::cout << "sbutitue dst: " << next_elem << std::endl;
#endif
                    result.push_back({ read_cond, elem, next_elem });
                }
            }
            if (!result.empty())
            {
                no_read_cond = simplifyUseTactic(cond && no_read_cond, true);
                if (!no_read_cond.is_false())
                {
                    result.push_back({ no_read_cond, elem, arr.base[read_index] });
                }
                return result;
            }
        }
    }
    else if (elem.decl().decl_kind() == Z3_OP_UNINTERPRETED && elem.decl().name().str() == "read")
    {
        return getSelectSubstitute(cond, elem.arg(0), elem.arg(1), visited);
    }
    else
    {
        for (uint32_t i = 0; i < elem.num_args(); i++)
        {
            auto result = getSelectSubstitute(cond, elem.arg(i), r, visited);
            if (!result.empty())
            {
                return result;
            }
        }
    }
    visited.emplace(elem);
    return {};
}

Summary SummaryIterativeSearcher::propagateRead(const Summary& summary)
{
    auto result = summary.copyBase();
    for (const auto& branch : summary.branches)
    {
        z3::expr cond = branch.cond;
        z3::expr elem = branch.elem;
        std::vector<Summary::WriteIndex> index = branch.index;
        z3::expr_vector src(z3ctx);
        z3::expr_vector dst(z3ctx);
        std::unordered_set<Z3_ast> visited;
        while (getReadSubstitute(src, dst, elem, visited))
        {
            visited.clear();
            elem = elem.substitute(src, dst).simplify();
            src.resize(0);
            dst.resize(0);
        }
        cond = cond.substitute(v_x, v_t);
        elem = elem.substitute(v_x, v_t);
        for (size_t j = 0; j < index.size(); j++)
        {
            index[j].func = index[j].func.substitute(v_x, v_t).simplify();
        }
        result.addBranch({ cond, elem, branch.layer, index, branch.skolem_x });
    }
    return result;
}

bool SummaryIterativeSearcher::getReadSubstitute(z3::expr_vector& src, z3::expr_vector& dst,
    z3::expr elem, std::unordered_set<Z3_ast>& visited)
{
    if (visited.count(elem) > 0)
    {
        return false;
    }
    if (elem.decl().decl_kind() == Z3_OP_UNINTERPRETED && elem.decl().name().str() == "read")
    {
        src.push_back(elem);
        dst.push_back(elem.arg(0));
        return true;
    }
    else
    {
        bool has_read = false;
        for (uint32_t i = 0; i < elem.num_args(); i++)
        {
            has_read = getReadSubstitute(src, dst, elem.arg(i), visited) || has_read;
        }
        if (!has_read)
        {
            visited.emplace(elem);
        }
        return has_read;
    }
}

Summary SummaryIterativeSearcher::nextTimeStep(const Summary& invar)
{
    Summary result = invar;
    for (auto& branch : result.branches)
    {
        branch.cond = simplifyUseTactic(branch.cond.substitute(v_t, dst_t));
        branch.elem = branch.elem.substitute(v_t, dst_t).simplify();
        for (size_t j = 0; j < branch.index.size(); j++)
        {
            branch.index[j].func = branch.index[j].func.substitute(v_t, dst_t).simplify();
        }
    }
    return result;
}

Summary SummaryIterativeSearcher::propagateSkolemX(const Summary& invar)
{
    Summary result = invar;
    for (auto& branch : result.branches)
    {
        z3::expr_vector dst_sk(z3ctx);
        dst_sk.push_back(branch.skolem_x.value());
        branch.cond = simplifyUseTactic(branch.cond.substitute(v_x, dst_sk));
        branch.elem = branch.elem.substitute(v_x, dst_sk).simplify();
    }
    return result;
}

Summary SummaryIterativeSearcher::propagateSkolemB(const Summary& invar)
{
    Summary result = invar;
    z3::expr_vector src_b(z3ctx);
    z3::expr_vector dst_sk(z3ctx);
    for (auto& branch : result.branches)
    {
        for (const auto& index : branch.index)
        {
            if (index.bound.has_value() && index.skolem_b.has_value())
            {
                src_b.push_back(index.bound.value());
                dst_sk.push_back(index.skolem_b.value());
            }
        }
    }
    for (auto& branch : result.branches)
    {
        for (uint32_t j = 0; j < branch.index.size(); j++)
        {
            branch.index[j].func = branch.index[j].func.substitute(v_t, v_x).simplify();
        }
        if (!src_b.empty())
        {
            branch.cond = simplifyUseTactic(branch.cond.substitute(src_b, dst_sk));
            branch.elem = branch.elem.substitute(src_b, dst_sk).simplify();
            for (uint32_t j = 0; j < branch.index.size(); j++)
            {
                branch.index[j].func = branch.index[j].func.substitute(src_b, dst_sk).simplify();
            }
        }
    }
    return result;
}

Summary SummaryIterativeSearcher::getPostCond(const Summary& invar)
{
    z3::expr_vector dst_0(z3ctx);
    dst_0.push_back(z3ctx.int_val(0));
    z3::expr_vector dst_1(z3ctx);
    dst_1.push_back(z3ctx.int_val(1));
    z3::expr_vector dst_times(z3ctx);
    dst_times.push_back(t_times);
    auto result = invar.copyBase();
    for (const auto& branch : invar.branches)
    {
        z3::expr cond = branch.cond;
        z3::expr elem = branch.elem;
        std::vector<Summary::WriteIndex> index = branch.index;
        cond = simplifyUseTactic(cond.substitute(v_t, dst_times));
        elem = elem.substitute(v_t, dst_times).simplify();
        z3::expr_vector dst_sk(z3ctx);
        dst_sk.push_back(branch.skolem_x.value());
        for (uint32_t j = 0; j < index.size(); j++)
        {
            index[j].func = index[j].func.substitute(v_x, dst_sk).simplify();
        }
        result.mergeBranch({ cond, elem, branch.layer, index });
    }
    return result;
}
