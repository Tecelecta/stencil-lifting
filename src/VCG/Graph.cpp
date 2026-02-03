#define VCG_EXPORTS

#include "Graph.h"
#include "ContainerHash.h"

#include <queue>
#include <algorithm>

#include <boost/pfr.hpp>

BOOST_PFR_HASH_EQ_NE_API(ValueCallPath)
BOOST_PFR_HASH_EQ_NE_API(SectionCallPath)
		
void Graph::clear()
{
	rootSectionVector.clear();
	sectionGuardVector.clear();
	operandsVector.clear();
	dependencyVector.clear();
	dataflowVector.clear();
	vertexSubgraphVector.clear();
	vertexSccVector.clear();
	calleeVector.clear();
	callerVector.clear();
	vertexNum = 0;
	subgraphNum = 0;
}

bool Graph::iterateSCC(SccAction& action) const
{
	for (const auto& scc : vertexSccVector)
	{
		assert(!scc.empty());
		if (isTrivialSCC(scc, dependencyVector))
		{
			// 平凡强连通分量：只有一个顶点并且不含自环
			if (!action.runOnTrivial(scc.front()))
			{
				return false;
			}
		}
		else if (!action.runOnTypical(scc))
		{
			return false;
		}
	}
	return true;
}

void Graph::saveRootSections(std::vector<Section*> rootSectionVector)
{
	clear();
	if (!rootSectionVector.empty())
	{
		assert(std::find(rootSectionVector.begin(), rootSectionVector.end(), nullptr) == rootSectionVector.end());
		context = rootSectionVector.front()->getContext();
		this->rootSectionVector = std::move(rootSectionVector);
		createSectionGuard();
	}
}

void Graph::createSectionGuard()
{
	sectionGuardVector.resize(rootSectionVector.size());
	for (size_t i = 0; i < rootSectionVector.size(); i++)
	{
		sectionGuardVector[i] = MarkedObjectGuard(rootSectionVector[i]);
	}
}

void Graph::subgraphTopologicalSort(vertex_mapping& f)
{
	auto subgraphSccVector = computeSCC(callerVector, calleeVector);
	for (uint32_t i = 0; i < subgraphSccVector.size(); i++)
	{
		if (!isTrivialSCC(subgraphSccVector[i], calleeVector))
		{
			throw std::runtime_error("Recusion is not supported");
		}
		f[subgraphSccVector[i].front()] = i;
	}
	calleeVector = transformEdgesByMapping(adjacency_list(std::move(calleeVector)), f);
	callerVector = getReverseGraph(calleeVector);
}

void GraphNoCallPath::clear()
{
	Graph::clear();
	VertexMapping<Value*>::clear();
	SubgraphMapping<Section*>::clear();
}

void GraphNoCallPath::load(std::vector<Section*> rootSectionVector)
{
	saveRootSections(std::move(rootSectionVector));
	if (this->rootSectionVector.empty())
	{
		return;
	}
	collectSection();
	collectVariable();

	// 收集顶点依赖信息
	operandsVector.resize(vertexVector.size());
	dependencyVector.resize(vertexVector.size());
	auto varNum = (uint32_t)vertexVector.size();
	for (uint32_t i = 0; i < varNum; i++)
	{
		if (auto resultValue = dynamic_cast<ResultValue*>(vertexAt(i)))
		{
			auto call = resultValue->getCall();
			if (subgraphMap.count(call->getCallee()) > 0)
			{
				addEdge(i, resultValue->getSrc(), true);
			}
			else
			{
				// 如果被调用的图片段未展开，则结果值认为依赖调用前传入的实参
				for (auto value : call->getArgumentVector())
				{
					addEdge(i, value, true);
				}
				for (auto value : call->getSpecializerVector())
				{
					addEdge(i, value, true);
				}
			}
		}
		else
		{
			// 普通变量依赖源操作数
			auto dependencies = vertexVector[i]->getDependencyVector();
			for (auto dependency : dependencies)
			{
				addEdge(i, dependency, true);
			}
		}
	}
	for (auto caller : subgraphVector)
	{
		for (auto call : caller->getSectionCallVector())
		{
			// 形参依赖实参
			auto callee = call->getCallee();
			if (subgraphMap.count(callee) > 0)
			{
				for (size_t i = 0; i < call->getArgumentVectorSize(); i++)
				{
					addEdge(vertexId(callee->getParameter(i)), call->getArgument(i), true);
				}
				if (auto callee_sectionGenerator = dynamic_cast<SectionGenerator*>(callee))
				{
					for (size_t i = 0; i < call->getSpecializerVectorSize(); i++)
					{
						addEdge(vertexId(callee_sectionGenerator->getTemplate(i)), call->getSpecializer(i), true);
					}
				}
			}
		}
		if (auto caller_sectionGenerator = dynamic_cast<SectionGenerator*>(caller))
		{
			for (auto inputValue : caller_sectionGenerator->getParameterVector())
			{
				for (auto templateValue : caller_sectionGenerator->getTemplateVector())
				{
					// 规定普通参数依赖模板参数
					addEdge(vertexId(inputValue), templateValue, false);
				}
			}
			for (auto boundValue : caller_sectionGenerator->getBoundVector())
			{
				for (auto templateValue : caller_sectionGenerator->getTemplateVector())
				{
					// 规定约束变元依赖模板参数
					addEdge(vertexId(boundValue), templateValue, false);
				}
			}
		}
	}

	vertexNum = (uint32_t)vertexVector.size();
	subgraphNum = (uint32_t)subgraphVector.size();
	sortAndUniqueEdges(dependencyVector);
	dataflowVector = getReverseGraph(dependencyVector);
	vertexSccVector = computeSCC(dataflowVector, dependencyVector);
}

void GraphNoCallPath::validate() const
{
	for (auto section : subgraphVector)
	{
		section->validate();
	}
	for (auto value : vertexVector)
	{
		if (value == nullptr)
		{
			throw std::runtime_error("value cannot be nullptr");
		}
	}
}

void GraphNoCallPath::collectVariable()
{
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		auto variableVector = subgraphVector[i]->getVariableVector();
		vertexVector.insert(vertexVector.end(), variableVector.begin(), variableVector.end());
		vertexSubgraphVector.insert(vertexSubgraphVector.end(), variableVector.size(), i);
	}
	for (uint32_t i = 0; i < vertexVector.size(); i++)
	{
		vertexMap.emplace(vertexVector[i], i);
	}
}

void GraphNoCallPath::createFilter(std::vector<bool>& vertexFilter, std::vector<bool>& subgraphFilter, vertex_list initUsefulList) const
{
	subgraphFilter.resize(getSubgraphNum(), false);
	bool changed;
	do {
		changed = false;
		vertexFilter = visitReachableSubGraph(getDependencyVector(), initUsefulList);
		for (uint32_t v = 0; v < getVertexNum(); v++)
		{
			if (vertexFilter.at(v) && dynamic_cast<VariableValue*>(vertexAt(v)) != nullptr)
			{
				uint32_t g = getSubgraphByVertex(v);
				if (!subgraphFilter.at(g))
				{
					subgraphFilter[g] = true;
					if (auto section = dynamic_cast<SectionGenerator*>(subgraphAt(g)))
					{
						for (auto param : section->getTemplateVector())
						{
							changed = true;
							initUsefulList.push_back(vertexId(param));
						}
						for (auto bound : section->getBoundVector())
						{
							changed = true;
							initUsefulList.push_back(vertexId(bound));
						}
					}
				}
			}
		}
	} while (changed);
	for (uint32_t g = 0; g < getSubgraphNum(); g++)
	{
		if (subgraphFilter.at(g))
		{
			if (auto section = dynamic_cast<SectionGenerator*>(subgraphAt(g)))
			{
				// 带控制流的语句必须保留子Section
				for (auto call : section->getSectionCallVector())
				{
					subgraphFilter[subgraphId(call->getCallee())] = true;
				}
			}
		}
	}
}

void GraphNoCallPath::addEdge(uint32_t vertex, Value* dependency, bool isOperand)
{
	assert(dependency != nullptr);
	if (dynamic_cast<VariableValue*>(dependency) == nullptr)
	{
		// 尝试添加新的顶点表示常量和无效值，新顶点号是当前顶点数量，已经存在则不重复添加
		auto n = (uint32_t)vertexVector.size();
		if (vertexMap.try_emplace(dependency, n).second)
		{
			vertexVector.push_back(dependency);
			operandsVector.emplace_back();
			dependencyVector.emplace_back();
		}
	}
	dependencyVector[vertex].push_back(vertexId(dependency));
	if (isOperand)
	{
		operandsVector[vertex].push_back(dependencyVector[vertex].back());
	}
}

void GraphOuterSection::collectSection()
{
	subgraphVector = this->rootSectionVector;
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		subgraphMap.emplace(subgraphVector[i], i);
	}
	calleeVector.resize(subgraphVector.size());
	callerVector.resize(subgraphVector.size());
}

void GraphOuterSection::cloneVariableBuilder(
	std::vector<std::vector<Value::Builder>>& builders,
	std::unordered_map<Value*, Value*>& valueMap,
	Value* value) const
{
	if (auto variableValue = dynamic_cast<VariableValue*>(value))
	{
		auto builder = variableValue->cloneBuilder();
		valueMap.emplace(variableValue, builder.getInstance());
		builders[subgraphId(variableValue->getSection())].emplace_back(std::move(builder));
	}
}

void GraphOuterSection::cloneVariableBuilders(
	std::vector<std::vector<Value::Builder>>& builders,
	std::unordered_map<Value*, Value*>& valueMap) const
{
	builders.resize(subgraphVector.size());
	for (auto value : vertexVector)
	{
		cloneVariableBuilder(builders, valueMap, value);
	}
}

void GraphOuterSection::cloneVariableBuilders(
	std::vector<std::vector<Value::Builder>>& builders,
	std::unordered_map<Value*, Value*>& valueMap,
	const vertex_list& usefulList) const
{
	builders.resize(subgraphVector.size());
	for (uint32_t i : usefulList)
	{
		cloneVariableBuilder(builders, valueMap, vertexVector[i]);
	}
}

void GraphOuterSection::splitPhiAndResult(const vertex_list& scc,
	std::vector<PhiValue*>& phiValueVector, std::vector<ResultValue*>& resultValueVector) const
{
	for (uint32_t vertex : scc)
	{
		if (auto phiValue = dynamic_cast<PhiValue*>(vertexAt(vertex)))
		{
			phiValueVector.push_back(phiValue);
		}
		else if (auto resultValue = dynamic_cast<ResultValue*>(vertexAt(vertex)))
		{
			resultValueVector.push_back(resultValue);
		}
		else
		{
			assert(false);
		}
	}
}

void GraphValueProjection::collectSection()
{
	std::queue<Section*> Q;
	subgraphVector = this->rootSectionVector;
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		subgraphMap.emplace(subgraphVector[i], i);
		Q.push(subgraphVector[i]);
	}
	calleeVector.resize(subgraphVector.size());

	// 广度优先搜索，找出所有Section
	while (!Q.empty())
	{
		auto caller = Q.front();
		Q.pop();
		for (auto call : caller->getSectionCallVector())
		{
			auto callee = call->getCallee();
			auto n = (uint32_t)subgraphVector.size();
			auto result = subgraphMap.try_emplace(callee, n);
			if (result.second)
			{
				subgraphVector.push_back(callee);
				calleeVector.emplace_back();
				Q.push(callee);
			}
			else
			{
				n = result.first->second;
			}
			calleeVector[subgraphId(caller)].push_back(n);
		}
	}
	callerVector = getReverseGraph(calleeVector);

	// 对Section拓扑排序，并检查递归
	vertex_mapping f(subgraphVector.size());
	subgraphTopologicalSort(f);
	reorder(f);
}

void GraphLoopAndBody::load(std::vector<Section*> rootSectionVector)
{
	assert(rootSectionVector.size() == 1);
	loadLoop(dynamic_cast<SectionGenerator*>(rootSectionVector[0]));
}

void GraphLoopAndBody::loadLoop(SectionGenerator* loop)
{
	assert(dynamic_cast<IterateLoop*>(loop) != nullptr
		|| dynamic_cast<ParallelLoop*>(loop) != nullptr);
	this->context = loop->getContext();
	this->loop = loop;
	this->body = dynamic_cast<SimpleSection*>(loop->getSectionCall(0)->getCallee());
	constantSpecializer.resize(loop->getTemplateVectorSize());
	GraphValueProjection::load({ loop });
}

void GraphLoopAndBody::collectSection()
{
	subgraphVector = { body, loop };
	subgraphMap = { { body, 0 }, { loop, 1 } };
	calleeVector.resize(2);
	calleeVector[1] = { 0 };
	callerVector.resize(2);
	callerVector[0] = { 1 };
}

void GraphCallPath::clear()
{
	Graph::clear();
	VertexMapping<ValueCallPath>::clear();
	SubgraphMapping<SectionCallPath>::clear();
}

void GraphCallPath::load(std::vector<Section*> rootSectionVector)
{
	saveRootSections(std::move(rootSectionVector));
	if (this->rootSectionVector.empty())
	{
		return;
	}

	for (auto root : this->rootSectionVector)
	{
		std::vector<SectionCall*> callVector;
		collectSectionAndVariable(root, callVector);
	}
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		subgraphMap.emplace(subgraphVector[i], i);
	}
	for (uint32_t i = 0; i < vertexVector.size(); i++)
	{
		vertexMap.emplace(vertexVector[i], i);
	}

	calleeVector.resize(subgraphVector.size());
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		auto path = subgraphAt(i);
		auto caller = path.section;
		for (auto call : caller->getSectionCallVector())
		{
			path.callVector.push_back(call);
			path.section = call->getCallee();
			if (subgraphMap.count(path) > 0)
			{
				calleeVector[i].push_back(subgraphId(path));
			}
			path.callVector.pop_back();
		}
	}
	callerVector = getReverseGraph(calleeVector);

	// 对Section拓扑排序，并检查递归
	vertex_mapping f(subgraphVector.size());
	subgraphTopologicalSort(f);
	reorder(f);
	vertexSubgraphVector.resize(vertexVector.size());
	for (uint32_t i = 0; i < subgraphVector.size(); i++)
	{
		auto variableVector = subgraphVector[i].section->getVariableVector();
		for (auto var : variableVector)
		{
			vertexSubgraphVector[vertexId({ var, subgraphVector[i].callVector })] = i;
		}
	}

	// 收集顶点依赖
	operandsVector.resize(vertexVector.size());
	dependencyVector.resize(vertexVector.size());
	auto varNum = (uint32_t)vertexVector.size();
	for (uint32_t i = 0; i < varNum; i++)
	{
		auto path = vertexAt(i);
		auto value = path.value;
		std::vector<SectionCall*> callVector(path.callVector);
		if (auto inputValue = dynamic_cast<InputValue*>(value))
		{
			// 由传参定义的依赖
			if (!inputValue->isTemplate())
			{
				if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(inputValue->getSection()))
				{
					for (auto templateValue : sectionGenerator->getTemplateVector())
					{
						// 规定普通参数依赖模板参数
						addEdge(vertexId({ inputValue, callVector }), templateValue, callVector, false);
					}
				}
			}
			if (!callVector.empty())
			{
				auto call = callVector.back();
				callVector.pop_back();
				if (inputValue->isTemplate())
				{
					addEdge(i, call->getSpecializer(inputValue->getIndex()), callVector, true);
				}
				else
				{
					addEdge(i, call->getArgument(inputValue->getIndex()), callVector, true);
				}
			}
		}
		else if (auto boundValue = dynamic_cast<BoundValue*>(value))
		{
			if (auto sectionGenerator = dynamic_cast<SectionGenerator*>(boundValue->getSection()))
			{
				for (auto templateValue : sectionGenerator->getTemplateVector())
				{
					// 规定约束变元依赖模板参数
					addEdge(vertexId({ boundValue, callVector }), templateValue, callVector, false);
				}
			}
		}
		else if (auto resultValue = dynamic_cast<ResultValue*>(value))
		{
			// 由返回定义的依赖
			callVector.push_back(resultValue->getCall());
			auto call = resultValue->getCall();
			if (subgraphMap.count(SectionCallPath{ call->getCallee(), callVector }) > 0)
			{
				addEdge(i, resultValue->getSrc(), callVector, true);
			}
			else
			{
				// 如果被调用的图片段未展开，则结果值认为依赖调用前传入的实参
				callVector.pop_back();
				for (auto value : call->getArgumentVector())
				{
					addEdge(i, value, callVector, true);
				}
				for (auto value : call->getSpecializerVector())
				{
					addEdge(i, value, callVector, true);
				}
			}
		}
		else if (value != nullptr)
		{
			// 由计算顶点定义的依赖
			auto dependencies = value->getDependencyVector();
			for (auto dependency : dependencies)
			{
				addEdge(i, dependency, callVector, true);
			}
		}
		else
		{
			assert(false);
		}
	}

	vertexNum = (uint32_t)vertexVector.size();
	subgraphNum = (uint32_t)subgraphVector.size();
	sortAndUniqueEdges(dependencyVector);
	dataflowVector = getReverseGraph(dependencyVector);
	vertexSccVector = computeSCC(dataflowVector, dependencyVector);
}

void GraphCallPath::validate() const
{
	for (const auto& call : subgraphVector)
	{
		call.section->validate();
	}
	for (const auto& path : vertexVector)
	{
		if (path.value == nullptr)
		{
			throw std::runtime_error("value cannot be nullptr");
		}
	}
}

void GraphCallPath::collectSectionAndVariable(Section* section, std::vector<SectionCall*>& callVector)
{
	subgraphVector.emplace_back(SectionCallPath{ section, callVector });
	auto variableVector = section->getVariableVector();
	for (auto value : variableVector)
	{
		auto iv = (uint32_t)vertexVector.size();
		vertexVector.emplace_back(ValueCallPath{ value, callVector });
	}
	for (auto call : section->getSectionCallVector())
	{
		auto callee = call->getCallee();
		if (expandSection(callee))
		{
			callVector.push_back(call);
			collectSectionAndVariable(callee, callVector);
			callVector.pop_back();
		}
	}
}

void GraphCallPath::addEdge(uint32_t vertex, Value* dependency, std::vector<SectionCall*> callVector, bool isOperand)
{
	assert(dependency != nullptr);
	if (!dynamic_cast<VariableValue*>(dependency))
	{
		// 尝试添加新的顶点表示常量和无效值，新顶点号是当前顶点数量，已经存在则不重复添加
		auto n = (uint32_t)vertexVector.size();
		ValueCallPath path = { dependency, {} };
		if (vertexMap.try_emplace(path, n).second)
		{
			vertexVector.push_back(path);
			operandsVector.emplace_back();
			dependencyVector.emplace_back();
		}
		// 常量没有调用路径
		dependencyVector[vertex].push_back(vertexId({ dependency, {} }));
	}
	else
	{
		// 变量需要区分调用路径
		dependencyVector[vertex].push_back(vertexId({ dependency, std::move(callVector) }));
	}
	if (isOperand)
	{
		operandsVector[vertex].push_back(dependencyVector[vertex].back());
	}
}
