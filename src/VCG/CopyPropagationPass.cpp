#define VCG_EXPORTS

#include "CopyPropagationPass.h"

AnalyzeCopyValuePass::AnalyzeCopyValuePass()
{
    defaultFunction = [](Value* value, uint32_t i) { return i; };

    visitOperationValue = [this](OperationValue* value, uint32_t i)
        {
            if (value->getOperation().getName().equals("copy"))
            {
                assert(value->getSrcVectorSize() == 1);
                return vertexEquClass[getOperandsByVertex(i)[0]];
            }
            if (value->getOperation().getName().equals("select"))
            {
                assert(value->getSrcVectorSize() == 3);
                if (vertexEquClass[getOperandsByVertex(i)[1]] == vertexEquClass[getOperandsByVertex(i)[2]])
                {
                    return vertexEquClass[getOperandsByVertex(i)[1]];
                }
                return i;
            }
            return i;
        };

    visitResultValue = [this](ResultValue* value, uint32_t i)
        {
            Value* src = value->getSrc();
            if (auto pass = dynamic_cast<CopyPropagationShallowPass*>(this))
            {
                src = pass->mapValueByParent(src);
                auto call = value->getCall();
                if (auto src_InputValue = dynamic_cast<InputValue*>(src))
                {
                    if (src_InputValue->isTemplate())
                    {
                        return vertexEquClass[vertexId(pass->mapValueByParent(call->getSpecializer(src_InputValue->getIndex())))];
                    }
                    else
                    {
                        return vertexEquClass[vertexId(pass->mapValueByParent(call->getArgument(src_InputValue->getIndex())))];
                    }
                }
            }
            return i;
        };

    visitPhiValue = [this](PhiValue* value, uint32_t i)
        {
            // 开头的不可以是回边
            auto init = vertexEquClass[getOperandsByVertex(i)[0]];
            for (uint32_t j = 1; j < value->getIncomingVectorSize(); j++)
            {
                auto other = vertexEquClass[getOperandsByVertex(i)[j]];
                if (other != i && other != init)
                {
                    return i;
                }
            }
            return init;
        };
}

void AnalyzeCopyValuePass::run()
{
    vertexEquClass.resize(getVertexNum(), ~0u);
    iterateSCC(*this);
}

bool AnalyzeCopyValuePass::runOnTrivial(uint32_t i)
{
    if (vertexFilter.empty() || vertexFilter.at(i))
    {
        vertexEquClass[i] = (*this)(vertexAt(i), i);
    }
    return true;
}

bool AnalyzeCopyValuePass::runOnTypical(const vertex_list& scc)
{
    if (vertexFilter.empty() || vertexFilter.at(scc[0]))
    {
        std::vector<PhiValue*> phiValueVector;
        std::vector<ResultValue*> resultValueVector;
        splitPhiAndResult(scc, phiValueVector, resultValueVector);
        // 迭代计算2次，必定收敛
        for (auto phiValue : phiValueVector)
        {
            uint32_t i = vertexId(phiValue);
            vertexEquClass[i] = i;
        }
        for (auto resultValue : resultValueVector)
        {
            uint32_t i = vertexId(resultValue);
            vertexEquClass[i] = visitResultValue(resultValue, i);
        }
        for (auto phiValue : phiValueVector)
        {
            uint32_t i = vertexId(phiValue);
            vertexEquClass[i] = visitPhiValue(phiValue, i);
        }
        for (auto resultValue : resultValueVector)
        {
            uint32_t i = vertexId(resultValue);
            vertexEquClass[i] = visitResultValue(resultValue, i);
        }
    }
    return true;
}

GraphOuterSection CopyPropagationShallowPass::run(GraphOuterSection src)
{
    setGraph(std::move(src));
    AnalyzeCopyValuePass::run();
    CopySectionShallowPass::run();
    return std::move(result);
}

void CopyPropagationDeepPass::runOnSubgraph(uint32_t i)
{
    CopyPropagationShallowPass subPass;
    subPass.parent = this;
    subPass.run(GraphOuterSection({ subgraphAt(i) }));
    updateResultVector(subPass);
}
