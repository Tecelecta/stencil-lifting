#include "CommonSubexprEliminationPass.h"

AnalyzeCommonSubexprPass::AnalyzeCommonSubexprPass()
    : equSetOperationValue(128, *this, *this), equSetResultValue(32, *this, *this), equSetPhiValue(32, *this, *this),
    equSetConstantValue(64, *this, *this), equSetInvalidValue(32, *this, *this)
{
    defaultFunction = [](Value* value, uint32_t i)
        {
            return i;
        };

    visitOperationValue = [this](OperationValue* value, uint32_t i)
        {
            return findOrReturnSelf(value, i, equSetOperationValue);
        };

    visitResultValue = [this](ResultValue* value, uint32_t i)
        {
            return findOrReturnSelf(value, i, equSetResultValue);
        };

    visitPhiValue = [this](PhiValue* value, uint32_t i)
        {
            return findOrReturnSelf(value, i, equSetPhiValue);
        };

    visitConstantValue = [this](ConstantValue* value, uint32_t i)
        {
            return findOrReturnSelf(value, i, equSetConstantValue);
        };

    visitInvalidValue = [this](InvalidValue* value, uint32_t i)
        {
            return findOrReturnSelf(value, i, equSetInvalidValue);
        };
}

void AnalyzeCommonSubexprPass::run()
{
    vertexEquClass.resize(getVertexNum());
    for (uint32_t i = 0; i < getVertexNum(); i++)
    {
        vertexEquClass[i] = i;
    }
    iterateSCC(*this);
}

bool AnalyzeCommonSubexprPass::runOnTrivial(uint32_t i)
{
    if (vertexFilter.empty() || vertexFilter.at(i))
    {
        vertexEquClass[i] = (*this)(vertexAt(i), i);
    }
    return true;
}

bool AnalyzeCommonSubexprPass::runOnTypical(const vertex_list& scc)
{
    if (vertexFilter.empty() || vertexFilter.at(scc[0]))
    {
        std::vector<PhiValue*> phiValueVector;
        std::vector<ResultValue*> resultValueVector;
        splitPhiAndResult(scc, phiValueVector, resultValueVector);
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
    }
    return true;
}

size_t AnalyzeCommonSubexprPass::OperationValueHasher::operator()(uint32_t a) const
{
    auto value = dynamic_cast<OperationValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    size_t result = value->getOperation().hash();
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        result ^= pass.vertexEquClass[pass.getOperandsByVertex(a)[i]];
    }
    return result;
}

bool AnalyzeCommonSubexprPass::OperationValueEqualTo::operator()(uint32_t a, uint32_t b) const
{
    auto value_a = dynamic_cast<OperationValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    auto value_b = dynamic_cast<OperationValue*>(pass.vertexAt(pass.vertexEquClass[b]));
    if (value_a->getOperation() != value_b->getOperation() ||
        pass.getOperandsByVertex(a).size() != pass.getOperandsByVertex(b).size())
    {
        return false;
    }
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        if (pass.vertexEquClass[pass.getOperandsByVertex(a)[i]] != pass.vertexEquClass[pass.getOperandsByVertex(b)[i]])
        {
            return false;
        }
    }
    return true;
}

size_t AnalyzeCommonSubexprPass::ResultValueHasher::operator()(uint32_t a) const
{
    auto value = dynamic_cast<ResultValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    size_t result = reinterpret_cast<size_t>(value->getCall()->getCallee());
    result ^= reinterpret_cast<size_t>(value->getSrc());
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        result ^= pass.vertexEquClass[pass.getOperandsByVertex(a)[i]];
    }
    return result;
}

bool AnalyzeCommonSubexprPass::ResultValueEqualTo::operator()(uint32_t a, uint32_t b) const
{
    auto value_a = dynamic_cast<ResultValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    auto value_b = dynamic_cast<ResultValue*>(pass.vertexAt(pass.vertexEquClass[b]));
    if (value_a->getCall()->getCallee() != value_b->getCall()->getCallee() ||
        value_a->getSrc() != value_b->getSrc() ||
        pass.getOperandsByVertex(a).size() != pass.getOperandsByVertex(b).size())
    {
        return false;
    }
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        if (pass.vertexEquClass[pass.getOperandsByVertex(a)[i]] != pass.vertexEquClass[pass.getOperandsByVertex(b)[i]])
        {
            return false;
        }
    }
    return true;
}

size_t AnalyzeCommonSubexprPass::PhiValueHasher::operator()(uint32_t a) const
{
    auto value = dynamic_cast<PhiValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    size_t result = 0;
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        result ^= pass.vertexEquClass[pass.getOperandsByVertex(a)[i]];
    }
    return result;
}

bool AnalyzeCommonSubexprPass::PhiValueEqualTo::operator()(uint32_t a, uint32_t b) const
{
    auto value_a = dynamic_cast<PhiValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    auto value_b = dynamic_cast<PhiValue*>(pass.vertexAt(pass.vertexEquClass[b]));
    if (value_a->getIncomingVectorSize() != value_b->getIncomingVectorSize())
    {
        return false;
    }
    for (size_t i = 0; i < pass.getOperandsByVertex(a).size(); i++)
    {
        if (pass.vertexEquClass[pass.getOperandsByVertex(a)[i]] != pass.vertexEquClass[pass.getOperandsByVertex(b)[i]])
        {
            return false;
        }
    }
    return true;
}

size_t AnalyzeCommonSubexprPass::ConstantValueHasher::operator()(uint32_t a) const
{
    auto value = dynamic_cast<ConstantValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    return value->getType().hash() ^ value->getValue().hash();
}

bool AnalyzeCommonSubexprPass::ConstantValueEqualTo::operator()(uint32_t a, uint32_t b) const
{
    auto value_a = dynamic_cast<ConstantValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    auto value_b = dynamic_cast<ConstantValue*>(pass.vertexAt(pass.vertexEquClass[b]));
    return value_a->getType() == value_b->getType() &&
        value_a->getValue() == value_b->getValue();
}

size_t AnalyzeCommonSubexprPass::InvalidValueHasher::operator()(uint32_t a) const
{
    auto value = dynamic_cast<InvalidValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    return value->getType().hash();
}

bool AnalyzeCommonSubexprPass::InvalidValueEqualTo::operator()(uint32_t a, uint32_t b) const
{
    auto value_a = dynamic_cast<InvalidValue*>(pass.vertexAt(pass.vertexEquClass[a]));
    auto value_b = dynamic_cast<InvalidValue*>(pass.vertexAt(pass.vertexEquClass[b]));
    return value_a->getType() == value_b->getType();
}

GraphOuterSection CommonSubexprEliminationShallowPass::run(GraphOuterSection src)
{
    setGraph(std::move(src));
    AnalyzeCommonSubexprPass::run();
    CopySectionShallowPass::run();
    return std::move(result);
}

void CommonSubexprEliminationDeepPass::runOnSubgraph(uint32_t i)
{
    CommonSubexprEliminationShallowPass subPass;
    subPass.parent = this;
    subPass.run(GraphOuterSection({ subgraphAt(i) }));
    updateResultVector(subPass);
}
