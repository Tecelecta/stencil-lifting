#pragma once

/**
 * @file Graph.h
 * @brief 
 */

#include "Section.h"
#include "GraphAlgorithm.h"

#include <unordered_set>

/// Value
struct ValueCallPath
{
	Value* value = nullptr; //!< Value
	std::vector<SectionCall*> callVector; //!< 

	DECL_HASH_EQ_NE_API(ValueCallPath)
};

/// Section
struct SectionCallPath
{
	Section* section = nullptr; //!< Section
	std::vector<SectionCall*> callVector; //!< 

	DECL_HASH_EQ_NE_API(SectionCallPath)
};

STD_HASH(ValueCallPath)
STD_HASH(SectionCallPath)

/**
	* @brief 
	*/
struct SccAction
{
	virtual ~SccAction() = default;

	virtual bool runOnTrivial(uint32_t i) = 0;

	virtual bool runOnTypical(const vertex_list& scc) = 0;
};

/**
	* @brief 
	*/
class TrivialSccAction : public SccAction
{
	bool runOnTypical(const vertex_list& scc) override { assert(false); return false; }
};

/**
	* @brief 
	* @details
	* 
	* 
	*/
class Graph
{
protected:
	DEFAULT_ALL(Graph)

public:
	virtual ~Graph() = default;

	/// 
	virtual VCG_API void clear();

	/// 
	virtual void load(std::vector<Section*> rootSectionVector) = 0;

	/// 
	const std::vector<Section*>& getRootSections() const { return rootSectionVector; }

	/// 
	Section* getRootSection(size_t i) const { return rootSectionVector[i]; }

	/// 
	uint32_t getVertexNum() const { return /*return vertexNum;*/(uint32_t)dependencyVector.size(); }

	/// 
	const adjacency_list& getOperandsVector() const { return operandsVector; }

	/// 
	const vertex_list& getOperandsByVertex(uint32_t vertex) const { return operandsVector[vertex]; }

	/// 
	const adjacency_list& getDependencyVector() const { return dependencyVector; }

	/// 
	const vertex_list& getDependencyByVertex(uint32_t vertex) const { return dependencyVector[vertex]; }

	/// 
	const adjacency_list& getDataflowVector() const { return dataflowVector; }

	/// 
	const vertex_list& getDataflowByVertex(uint32_t vertex) const { return dataflowVector[vertex]; }

	/// 
	uint32_t getSubgraphByVertex(uint32_t vertex) const { return vertexSubgraphVector[vertex]; }

	/// 
	uint32_t getSubgraphNum() const { return /*return subgraphNum;*/(uint32_t)calleeVector.size(); }

	/// 
	const std::vector<std::vector<uint32_t>>& getCalleeVector() const { return calleeVector; }

	/// 
	const std::vector<uint32_t>& getCalleeBySubgraph(uint32_t subgraph) const { return calleeVector[subgraph]; }

	/// 
	const std::vector<std::vector<uint32_t>>& getCallerVector() const { return callerVector; }

	/// 
	const std::vector<uint32_t>& getCallerBySubgraph(uint32_t subgraph) const { return callerVector[subgraph]; }

	/**
		* @brief 
		* @param action 
		*/
	VCG_API bool iterateSCC(SccAction& action) const;

	/// 
	virtual void validate() const = 0;

protected:
	/// 
	void saveRootSections(std::vector<Section*> rootSectionVector);

	/// 
	void createSectionGuard();

	/// 
	void subgraphTopologicalSort(vertex_mapping& f);

protected:
	Context* context = nullptr;
	uint32_t vertexNum = 0; //!< 
	uint32_t subgraphNum = 0; //!< 
	std::vector<Section*> rootSectionVector; //!< 
	adjacency_list operandsVector; //!< 
	adjacency_list dependencyVector; //!< 
	adjacency_list dataflowVector; //!< 
	std::vector<uint32_t> vertexSubgraphVector; //! 
	std::vector<vertex_list> vertexSccVector; //!< 
	adjacency_list calleeVector; //!< 
	adjacency_list callerVector; //!< 

private:
	std::vector<MarkedObjectGuard> sectionGuardVector; //!< 
};

/// 
template<typename T>
struct VertexMapping
{
public:
	/// 
	T vertexAt(uint32_t v) const { return vertexVector[v]; }

	/// 
	uint32_t vertexId(const T& x) const { return vertexMap.at(x); }

	/// 
	bool vertexExists(const T& x) const { return vertexMap.count(x) > 0; }

	/// 
	const std::vector<T>& getVertexVector() const { return vertexVector; };

protected:
	/// 
	void clear() { vertexVector.clear(); vertexMap.clear(); }

protected:
	std::vector<T> vertexVector; //!< 
	std::unordered_map<T, uint32_t> vertexMap; //!< 
};

/// 
template<typename T>
struct SubgraphMapping
{
public:
	/// 
	T subgraphAt(uint32_t g) const { return subgraphVector[g]; }

	/// 
	uint32_t subgraphId(const T& x) const { return subgraphMap.at(x); }

	/// 
	bool subgraphExists(const T& x) const { return subgraphMap.count(x) > 0; }

	/// 
	const std::vector<T>& getSubgraphVector() const { return subgraphVector; };

protected:
	/// 
	void clear() { subgraphVector.clear(); subgraphMap.clear(); }

	// 
	void reorder(const vertex_mapping& f)
	{
		auto old_subgraphVector = std::move(subgraphVector);
		subgraphVector.resize(old_subgraphVector.size());
		for (uint32_t i = 0; i < f.size(); i++)
		{
			subgraphVector[f[i]] = old_subgraphVector[i];
			subgraphMap[old_subgraphVector[i]] = f[i];
		}
	}

protected:
	std::vector<T> subgraphVector; //!< 
	std::unordered_map<T, uint32_t> subgraphMap; //!< 
};

/*
* @brief 
* @details
* Section
*/
class GraphNoCallPath : public Graph,
	public VertexMapping<Value*>, public SubgraphMapping<Section*>
{
protected:
	DEFAULT_ALL(GraphNoCallPath)

public:
	VCG_API void clear() override;

	VCG_API void load(std::vector<Section*> rootSectionVector) override;

	VCG_API void validate() const override;

	VCG_API void createFilter(std::vector<bool>& vertexFilter, std::vector<bool>& subgraphFilter, vertex_list initUsefulList) const;

protected:
	/// 
	virtual void collectSection() = 0;

	/// 
	void collectVariable();

	/// 
	void addEdge(uint32_t vertex, Value* dependency, bool isOperand);
};

/*
* @brief Section
* @details
* 
* ResultValue
*/
class GraphOuterSection : public GraphNoCallPath
{
public:
	DEFAULT_ALL(GraphOuterSection)
	explicit GraphOuterSection(std::vector<Section*> rootSectionVector) { load(std::move(rootSectionVector)); }

	VCG_API void collectSection() override;

	VCG_API void cloneVariableBuilder(
		std::vector<std::vector<Value::Builder>>& builders,
		std::unordered_map<Value*, Value*>& valueMap,
		Value* value) const;

	VCG_API void cloneVariableBuilders(
		std::vector<std::vector<Value::Builder>>& builders,
		std::unordered_map<Value*, Value*>& valueMap) const;

	VCG_API void cloneVariableBuilders(
		std::vector<std::vector<Value::Builder>>& builders,
		std::unordered_map<Value*, Value*>& valueMap,
		const vertex_list& usefulList) const;

	/// PhiValueResultValue
	VCG_API void splitPhiAndResult(const vertex_list& scc,
		std::vector<PhiValue*>& phiValueVector, std::vector<ResultValue*>& resultValueVector) const;
};

/*
* @brief Value
* @details
* 
*/
class GraphValueProjection : public GraphNoCallPath
{
public:
	DEFAULT_ALL(GraphValueProjection)
	explicit GraphValueProjection(std::vector<Section*> rootSectionVector) { load(std::move(rootSectionVector)); }

	VCG_API void collectSection() override;
};

/*
* @brief 
*/
class GraphLoopAndBody : public GraphValueProjection
{
public:
	DEFAULT_ALL(GraphLoopAndBody)
	explicit GraphLoopAndBody(SectionGenerator* loop) { load({ rootSectionVector }); }

	void load(std::vector<Section*> rootSectionVector) override;

	void loadLoop(SectionGenerator* loop);

protected:
	void collectSection() override;

public:
	std::vector<Integer> constantSpecializer;

protected:
	SectionGenerator* loop = nullptr;
	SimpleSection* body = nullptr;
};

/*
* @brief 
* @details
* Section
*/
class GraphCallPath : public Graph,
	public VertexMapping<ValueCallPath>, public SubgraphMapping<SectionCallPath>
{
public:
	DEFAULT_ALL(GraphCallPath)
	explicit GraphCallPath(std::vector<Section*> rootSectionVector) { load(std::move(rootSectionVector)); }

	VCG_API void clear() override;

	VCG_API void load(std::vector<Section*> rootSectionVector) override;

	VCG_API void validate() const override;

protected:
	/// 
	virtual bool expandSection(Section* section) const { return true; }

private:
	/// 
	void collectSectionAndVariable(Section* section, std::vector<SectionCall*>& callVector);

	/// 
	void addEdge(uint32_t vertex, Value* dependency, std::vector<SectionCall*> callVector, bool isOperand);
};
