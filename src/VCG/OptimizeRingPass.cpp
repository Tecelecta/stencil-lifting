#define VCG_EXPORTS

#include "OptimizeRingPass.h"
#include "Context.h"
#include "ContainerHash.h"

AnalyzeRingPass::AnalyzeRingPass()
{
    defaultFunction = [this](Value* value, uint32_t i)
        {
            ringNormalForms[i] = { {
                GroupNormalForm{ { value, value->getContext()->getInteger(1) } },
                value->getContext()->getInteger(1)
            } };
        };

    auto deal_with_operation = [this](Operation op, const std::vector<Value*>& srcVector, uint32_t i)
        {
            if (op.getName().equals("copy"))
            {
                ringNormalForms[i] = ringNormalForms[getOperandsByVertex(i)[0]];
                return true;
            }
            if (op == addOp)
            {
                ringNormalForms[i].clear();
                for (auto src : srcVector)
                {
                    uint32_t j = vertexId(src);
                    ringNormalForms[i].insert(ringNormalForms[i].end(), ringNormalForms[j].begin(), ringNormalForms[j].end());
                }
                mergeSameElement(ringNormalForms[i], *this); // 
                return true;
            }
            if (op == subOp || op == negOp)
            {
                uint32_t right = vertexId(srcVector.back());
                ringNormalForms[i].resize(ringNormalForms[right].size());
                std::transform(ringNormalForms[right].begin(), ringNormalForms[right].end(), ringNormalForms[i].begin(),
                    [](const auto& term)
                    {
                        return std::make_pair(term.first, -term.second);
                    });
                if (op == subOp)
                {
                    uint32_t left = vertexId(srcVector.front());
                    ringNormalForms[i].insert(ringNormalForms[i].begin(), ringNormalForms[left].begin(), ringNormalForms[left].end());
                }
                mergeSameElement(ringNormalForms[i], *this); // 
                return true;
            }
            if (op == mulOp)
            {
                ringNormalForms[i] = { { GroupNormalForm{}, op.getContext()->getInteger(1) } }; // 10
                for (auto src : srcVector)
                {
                    RingNormalForm dst;
                    uint32_t j = vertexId(src);
                    // 
                    for (const auto& [term1, coef1] : ringNormalForms[i])
                    {
                        for (const auto& [term2, coef2] : ringNormalForms[j])
                        {
                            GroupNormalForm term = term1;
                            term.insert(term.end(), term2.begin(), term2.end());
                            isCommutative ? AnalyzeGroupPass::mergeSameElement(term, *this) : AnalyzeGroupPass::mergeAdjacentSameElement(term);
                            Integer coef = coef1 * coef2;
                            dst.emplace_back(term, coef);
                        }
                    }
                    ringNormalForms[i] = std::move(dst);
                    if (ringNormalForms[i].empty())
                    {
                        return true; // 00
                    }
                }
                return true;
            }
            return false;
        };

    visitOperationValue = [this, deal_with_operation](OperationValue* value, uint32_t i)
        {
            if (!deal_with_operation(value->getOperation(), value->getSrcVector(), i))
            {
                defaultFunction(value, i);
            }
        };

    visitConstantValue = [this](ConstantValue* value, uint32_t i)
        {
            if (value->getType().canBeArgument(elemType))
            {
                if (value->getValue() == zero->getValue())
                {
                    ringNormalForms[i].clear();
                }
                else if (value->getValue() == one->getValue())
                {
                    ringNormalForms[i] = { { GroupNormalForm{}, value->getContext()->getInteger(1) } };
                }
                else
                {
                    defaultFunction(value, i);
                }
            }
            else
            {
                defaultFunction(value, i);
            }
        };
}

void AnalyzeRingPass::run()
{
    ringNormalForms.resize(getVertexNum());
    iterateSCC(*this);
}

void AnalyzeRingPass::mergeSameElement(RingNormalForm& ring, const GraphNoCallPath& graph)
{
    auto size = ring.size();
    RingNormalForm src = std::move(ring);
    std::unordered_map<GroupNormalForm, Integer> value_num_map;
    for (const auto& elem : src)
    {
        auto result = value_num_map.try_emplace(elem.first, elem.second);
        if (!result.second)
        {
            result.first->second += elem.second;
        }
    }
    ring = {};
    for (const auto& elem : value_num_map)
    {
        if (!elem.second.isZero())
        {
            ring.push_back(elem);
        }
    }
    /*std::sort(ring.begin(), ring.end(), [&graph](const auto& a, const auto& b)
        {
            return graph.vertexId(a.first) < graph.vertexId(b.first);
        });*/
}

bool AnalyzeRingPass::runOnTrivial(uint32_t i)
{
    if (vertexFilter.empty() || vertexFilter.at(i))
    {
        (*this)(vertexAt(i), i);
    }
    return true;
}

bool AnalyzeRingPass::runOnTypical(const vertex_list& scc)
{
    if (vertexFilter.empty() || vertexFilter.at(scc[0]))
    {
        for (uint32_t i : scc)
        {
            (*this)(vertexAt(i), i);
        }
    }
    return true;
}

ExtractRingShallowPass::ExtractRingShallowPass()
{
    createValueOperandsVisitor.visitOperationValue = [this](OperationValue* value,
        Value::Builder valueBuilder, Section::Builder sectionBuilder)
        {
            OperationValue::Builder builder;
            builder.setInstance(valueBuilder);
            SimpleSection::Builder simpleSectionBuilder;
            simpleSectionBuilder.setInstance(sectionBuilder);
            const auto& ring = ringNormalForms[vertexId(value)];
            auto context = value->getContext();
            if (ring.empty())
            {
                // 
                builder.setOperationAndType(context->getCopyOperation(zero->getType()));
                builder.setSrcVector({ zero });
            }
            else if (ring.size() == 1)
            {
                // 
                const auto& term = ring[0].first;
                const auto& coef = ring[0].second;
                if (coef.getData() == 1)
                {                      
                    if (term.size() == 1)
                    {
                        const auto& factor = term[0].first;
                        const auto& exponent = term[0].second;
                        if (exponent.getData() == 1 && factor == value)
                        {
                            // 
                            for (size_t i = 0; i < value->getSrcVectorSize(); i++)
                            {
                                builder.setSrc(i, mapValue(value->getSrc(i)));
                            }
                        }
                        else
                        {
                            // 
                            createFactor(context, *this, factor, exponent, builder, powOp);
                        }
                    }
                    else
                    {
                        // 
                        createTerm(context, simpleSectionBuilder, term, context->getInteger(1), builder, powOp, mulOp, negOp, scaOp);
                    }
                }
                else
                {
                    // 1
                    createTerm(context, simpleSectionBuilder, term, coef, builder, powOp, mulOp, negOp, scaOp);
                }
            }
            else
            {
                builder.setOperationAndType(context->getOperation(addOp.getName(),
                    std::vector<Type>(ring.size(), elemType), addOp.getReturnType()));
                builder.setSrcVectorSize(ring.size());
                for (size_t i = 0; i < ring.size(); i++)
                {
                    const auto& term = ring[i].first;
                    const auto& coef = ring[i].second;
                    OperationValue::Builder termBuilder;
                    termBuilder.createInstance(context);
                    auto src = createTerm(context, simpleSectionBuilder, term, coef, termBuilder, powOp, mulOp, negOp, scaOp);
                    simpleSectionBuilder.addOperationValue(termBuilder);
                    builder.setSrc(i, src);
                }
            }
            simpleSectionBuilder.addOperationValue(builder);
        };
}

GraphOuterSection ExtractRingShallowPass::run(GraphOuterSection src)
{
    setGraph(std::move(src));
    AnalyzeRingPass::run();
    CopySectionShallowPass::run();
    return std::move(result);
}

Value* ExtractRingShallowPass::createFactor(Context* context, const CopySectionShallowPass& pass, Value* factor, Integer exponent,
    OperationValue::Builder& builder, Operation powOp)
{
    if (exponent.getData() == 1)
    {
        builder.setOperationAndType(context->getCopyOperation(factor->getType()));
        builder.setSrcVector({ pass.mapValue(factor) });
    }
    else
    {
        builder.setOperationAndType(powOp);
        auto times = context->createConstantValue(context->getIntegerType(), exponent);
        builder.setSrcVector({ pass.mapValue(factor), times });
    }
    return builder.getInstance();
}

Value* ExtractRingShallowPass::createTerm(Context* context, SimpleSection::Builder& simpleSectionBuilder, const GroupNormalForm& term, Integer coef,
    OperationValue::Builder& builder, Operation powOp, Operation mulOp, Operation negOp, Operation scaOp)
{
    if (coef.getData() == 1)
    {
        if (term.empty())
        {
            // 
            builder.setOperationAndType(scaOp);
            auto times = context->createConstantValue(context->getIntegerType(), coef);
            builder.setSrcVector({ times, one });
        }
        else if (term.size() == 1)
        {
            // 
            const auto& factor = term[0].first;
            const auto& exponent = term[0].second;
            createFactor(context, *this, factor, exponent, builder, powOp);
        }
        else
        {
            // 
            builder.setOperationAndType(context->getOperation(mulOp.getName(),
                std::vector<Type>(term.size(), elemType), addOp.getReturnType()));
            builder.setSrcVectorSize(term.size());
            for (size_t i = 0; i < term.size(); i++)
            {
                const auto& factor = term[i].first;
                const auto& exponent = term[i].second;
                if (exponent.getData() == 1)
                {
                    builder.setSrc(i, mapValue(factor));
                }
                else
                {
                    OperationValue::Builder factorBuilder;
                    factorBuilder.createInstance(context);
                    auto result = createFactor(context, *this, factor, exponent, factorBuilder, powOp);
                    simpleSectionBuilder.addOperationValue(factorBuilder);
                    builder.setSrc(i, result);
                }
            }
        }
    }
    else
    {
        OperationValue::Builder factorBuilder;
        factorBuilder.createInstance(context);
        auto factor = createTerm(context, simpleSectionBuilder, term, context->getInteger(1), factorBuilder, powOp, mulOp, negOp, scaOp);
        simpleSectionBuilder.addOperationValue(factorBuilder);
        if (coef.getData() == -1)
        {
            builder.setOperationAndType(negOp);
            builder.setSrcVector({ factor });
        }
        else
        {
            builder.setOperationAndType(scaOp);
            auto times = context->createConstantValue(context->getIntegerType(), coef);
            builder.setSrcVector({ times, factor });
        }
    }
    return builder.getInstance();
}

void ExtractRingDeepPass::runOnSubgraph(uint32_t i)
{
    ExtractRingShallowPass subPass;
    static_cast<RingAlgebraSystem&>(subPass) = static_cast<const RingAlgebraSystem&>(*this);
    subPass.parent = this;
    subPass.run(GraphOuterSection({ subgraphAt(i) }));
    updateResultVector(subPass);
}
