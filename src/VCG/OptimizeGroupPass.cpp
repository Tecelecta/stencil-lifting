#define VCG_EXPORTS

#include "OptimizeGroupPass.h"
#include "Context.h"

AnalyzeGroupPass::AnalyzeGroupPass()
{
    defaultFunction = [this](Value* value, uint32_t i)
        {
            groupNormalForms[i] = { { value, value->getContext()->getInteger(1) } };
        };

    auto deal_with_operation = [this](Operation op, const std::vector<Value*>& srcVector, uint32_t i)
        {
            if (op.getName().equals("copy"))
            {
                groupNormalForms[i] = groupNormalForms[getOperandsByVertex(i)[0]];
                return true;
            }
            if (op == addOp)
            {
                groupNormalForms[i].clear();
                for (auto src : srcVector)
                {
                    uint32_t j = vertexId(src);
                    groupNormalForms[i].insert(groupNormalForms[i].end(), groupNormalForms[j].begin(), groupNormalForms[j].end());
                }
                isAbelian ? mergeSameElement(groupNormalForms[i], *this) : mergeAdjacentSameElement(groupNormalForms[i]);
                return true;
            }
            if (op == subOp || op == negOp)
            {
                uint32_t right = vertexId(srcVector.back());
                groupNormalForms[i].resize(groupNormalForms[right].size());
                std::transform(groupNormalForms[right].begin(), groupNormalForms[right].end(), groupNormalForms[i].begin(),
                    [](const auto& term)
                    {
                        return std::make_pair(term.first, -term.second);
                    });
                if (op == subOp)
                {
                    uint32_t left = vertexId(srcVector.front());
                    groupNormalForms[i].insert(groupNormalForms[i].begin(), groupNormalForms[left].begin(), groupNormalForms[left].end());
                }
                isAbelian ? mergeSameElement(groupNormalForms[i], *this) : mergeAdjacentSameElement(groupNormalForms[i]);
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
            if (value->getType().canBeArgument(elemType) && value->getValue() == zero->getValue())
            {
                groupNormalForms[i].clear();
            }
            else
            {
                defaultFunction(value, i);
            }
        };
}

void AnalyzeGroupPass::run()
{
    groupNormalForms.resize(getVertexNum());
    iterateSCC(*this);
}

void AnalyzeGroupPass::mergeSameElement(GroupNormalForm& group, const GraphNoCallPath& graph)
{
    auto size = group.size();
    GroupNormalForm src = std::move(group);
    std::unordered_map<Value*, Integer> value_num_map;
    for (const auto& elem : src)
    {
        auto result = value_num_map.try_emplace(elem.first, elem.second);
        if (!result.second)
        {
            result.first->second += elem.second;
        }
    }
    group = {};
    for (const auto& elem : value_num_map)
    {
        if (!elem.second.isZero())
        {
            group.push_back(elem);
        }
    }
    std::sort(group.begin(), group.end(), [&graph](const auto& a, const auto& b)
        {
            return graph.vertexId(a.first) < graph.vertexId(b.first);
        });
}

void AnalyzeGroupPass::mergeAdjacentSameElement(GroupNormalForm& group)
{
    auto size = group.size();
    GroupNormalForm src = std::move(group);
    GroupNormalForm dst;
    std::pair<Value*, Integer> last = { nullptr, Integer() };
    for (const auto& elem : src)
    {
        if (last.first == elem.first)
        {
            last.second += elem.second;
        }
        else
        {
            if (last.first != nullptr)
            {
                dst.push_back(last);
            }
            last = elem;
        }
    }
    dst.push_back(last);
    group = {};
    for (const auto& elem : dst)
    {
        if (!elem.second.isZero())
        {
            group.push_back(elem);
        }
    }
}

bool AnalyzeGroupPass::runOnTrivial(uint32_t i)
{
    if (vertexFilter.empty() || vertexFilter.at(i))
    {
        (*this)(vertexAt(i), i);
    }
    return true;
}

bool AnalyzeGroupPass::runOnTypical(const vertex_list& scc)
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

ExtractGroupShallowPass::ExtractGroupShallowPass()
{
    createValueOperandsVisitor.visitOperationValue = [this](OperationValue* value,
        Value::Builder valueBuilder, Section::Builder sectionBuilder)
        {
            OperationValue::Builder builder;
            builder.setInstance(valueBuilder);
            SimpleSection::Builder simpleSectionBuilder;
            simpleSectionBuilder.setInstance(sectionBuilder);
            const auto& group = groupNormalForms[vertexId(value)];
            auto context = value->getContext();
            if (group.empty())
            {
                builder.setOperationAndType(context->getCopyOperation(zero->getType()));
                builder.setSrcVector({ zero });
            }
            else if (group.size() == 1)
            {
                const auto& term = group[0].first;
                const auto& coef = group[0].second;
                if (coef.getData() == 1 && term == value)
                {
                    for (size_t i = 0; i < value->getSrcVectorSize(); i++)
                    {
                        builder.setSrc(i, mapValue(value->getSrc(i)));
                    }
                }
                else
                {
                    createTerm(context, *this, term, coef, builder, negOp, scaOp);
                }
            }
            else if (group.size() == 2 && group[0].second.getData() == 1 && group[1].second.getData() == -1)
            {
                builder.setOperationAndType(subOp);
                builder.setSrcVector({ mapValue(group[0].first), mapValue(group[1].first) });
            }
            else if (group.size() == 2 && group[0].second.getData() == -1 && group[1].second.getData() == 1)
            {
                builder.setOperationAndType(subOp);
                builder.setSrcVector({ mapValue(group[1].first), mapValue(group[0].first) });
            }
            else
            {
                builder.setOperationAndType(context->getOperation(addOp.getName(),
                    std::vector<Type>(group.size(), elemType), addOp.getReturnType()));
                builder.setSrcVectorSize(group.size());
                for (size_t i = 0; i < group.size(); i++)
                {
                    const auto& term = group[i].first;
                    const auto& coef = group[i].second;
                    if (coef.getData() == 1)
                    {
                        builder.setSrc(i, mapValue(term));
                    }
                    else
                    {
                        OperationValue::Builder termBuilder;
                        termBuilder.createInstance(context);
                        auto src = createTerm(context, *this, term, coef, termBuilder, negOp, scaOp);
                        simpleSectionBuilder.addOperationValue(termBuilder);
                        builder.setSrc(i, src);
                    }
                }
            }
            simpleSectionBuilder.addOperationValue(builder);
        };
}

GraphOuterSection ExtractGroupShallowPass::run(GraphOuterSection src)
{
    setGraph(std::move(src));
    AnalyzeGroupPass::run();
    CopySectionShallowPass::run();
    return std::move(result);
}

Value* ExtractGroupShallowPass::createTerm(Context* context, const CopySectionShallowPass& pass, Value* term, Integer coef,
    OperationValue::Builder& builder, Operation negOp, Operation scaOp)
{
    if (coef.getData() == 1)
    {
        builder.setOperationAndType(context->getCopyOperation(term->getType()));
        builder.setSrcVector({ pass.mapValue(term) });
    }
    else if (coef.getData() == -1)
    {
        builder.setOperationAndType(negOp);
        builder.setSrcVector({ pass.mapValue(term) });
    }
    else
    {
        builder.setOperationAndType(scaOp);
        auto times = context->createConstantValue(context->getIntegerType(), coef);
        builder.setSrcVector({ times, pass.mapValue(term) });
    }
    return builder.getInstance();
}

void ExtractGroupDeepPass::runOnSubgraph(uint32_t i)
{
    ExtractGroupShallowPass subPass;
    static_cast<GroupAlgebraSystem&>(subPass) = static_cast<const GroupAlgebraSystem&>(*this);
    subPass.parent = this;
    subPass.run(GraphOuterSection({ subgraphAt(i) }));
    updateResultVector(subPass);
}
