#pragma once

/**
 * @file Graph.h
 * @brief 定义计算图去掉属性后，剩下的顶点和边的结构
 */

#include "Section.h"
#include "GraphAlgorithm.h"

#include <unordered_set>

/// 对一个Value的调用路径
struct ValueCallPath
{
	Value* value = nullptr; //!< 源操作数是哪个Value
	std::vector<SectionCall*> callVector; //!< 逐层调用的路径，不包括最外层

	DECL_HASH_EQ_NE_API(ValueCallPath)
};

/// 对一个Section的调用路径
struct SectionCallPath
{
	Section* section = nullptr; //!< 源操作数是哪个Section
	std::vector<SectionCall*> callVector; //!< 逐层调用的路径，不包括最外层

	DECL_HASH_EQ_NE_API(SectionCallPath)
};

STD_HASH(ValueCallPath)
STD_HASH(SectionCallPath)

/**
	* @brief 定义遍历强连通分量时执行的操作
	*/
struct SccAction
{
	virtual ~SccAction() = default;

	virtual bool runOnTrivial(uint32_t i) = 0;

	virtual bool runOnTypical(const vertex_list& scc) = 0;
};

/**
	* @brief 仅允许遍历平凡强连通分量
	*/
class TrivialSccAction : public SccAction
{
	bool runOnTypical(const vertex_list& scc) override { assert(false); return false; }
};

/**
	* @brief 计算图的结构
	* @details
	* 计算图按某种方式遍历去掉属性后，剩下的顶点和边的结构
	* 对于投影图，得到缩点后的图结构
	*/
class Graph
{
protected:
	DEFAULT_ALL(Graph)

public:
	virtual ~Graph() = default;

	/// 清空图结构
	virtual VCG_API void clear();

	/// 以若干个片段为根进行遍历，加载图结构
	virtual void load(std::vector<Section*> rootSectionVector) = 0;

	/// 获取位于根部的片段
	const std::vector<Section*>& getRootSections() const { return rootSectionVector; }

	/// 按索引获取某个位于根部的片段
	Section* getRootSection(size_t i) const { return rootSectionVector[i]; }

	/// 获取顶点数量
	uint32_t getVertexNum() const { return /*return vertexNum;*/(uint32_t)dependencyVector.size(); }

	/// 获取操作数的顶点号邻接表
	const adjacency_list& getOperandsVector() const { return operandsVector; }

	/// 获取某个顶点操作数的顶点号
	const vertex_list& getOperandsByVertex(uint32_t vertex) const { return operandsVector[vertex]; }

	/// 获取依赖关系邻接表
	const adjacency_list& getDependencyVector() const { return dependencyVector; }

	/// 获取某个顶点依赖的顶点
	const vertex_list& getDependencyByVertex(uint32_t vertex) const { return dependencyVector[vertex]; }

	/// 获取数据流邻接表
	const adjacency_list& getDataflowVector() const { return dataflowVector; }

	/// 获取某个顶点数据流向的顶点
	const vertex_list& getDataflowByVertex(uint32_t vertex) const { return dataflowVector[vertex]; }

	/// 获取某个顶点属于哪个子图
	uint32_t getSubgraphByVertex(uint32_t vertex) const { return vertexSubgraphVector[vertex]; }

	/// 获取子图数量
	uint32_t getSubgraphNum() const { return /*return subgraphNum;*/(uint32_t)calleeVector.size(); }

	/// 获取调用关系邻接表
	const std::vector<std::vector<uint32_t>>& getCalleeVector() const { return calleeVector; }

	/// 获取某个子图所调用的子图
	const std::vector<uint32_t>& getCalleeBySubgraph(uint32_t subgraph) const { return calleeVector[subgraph]; }

	/// 获取被调用关系邻接表
	const std::vector<std::vector<uint32_t>>& getCallerVector() const { return callerVector; }

	/// 获取某个子图被调用的子图
	const std::vector<uint32_t>& getCallerBySubgraph(uint32_t subgraph) const { return callerVector[subgraph]; }

	/**
		* @brief 依次访问每个强连通分量
		* @param action 访问强连通分量时，执行的操作
		*/
	VCG_API bool iterateSCC(SccAction& action) const;

	/// 检查是否有效
	virtual void validate() const = 0;

protected:
	/// 保存位于根部的片段
	void saveRootSections(std::vector<Section*> rootSectionVector);

	/// 创建图片段守卫
	void createSectionGuard();

	/// 子图拓扑排序
	void subgraphTopologicalSort(vertex_mapping& f);

protected:
	Context* context = nullptr;
	uint32_t vertexNum = 0; //!< 顶点数量
	uint32_t subgraphNum = 0; //!< 子图数量
	std::vector<Section*> rootSectionVector; //!< 保证这些图片段引用的内存不被回收
	adjacency_list operandsVector; //!< 每个顶点的操作数的顶点号
	adjacency_list dependencyVector; //!< 每个顶点所依赖的其他顶点
	adjacency_list dataflowVector; //!< 每个顶点的数据流向的其他顶点
	std::vector<uint32_t> vertexSubgraphVector; //! 每个顶点属于哪个子图
	std::vector<vertex_list> vertexSccVector; //!< 顶点构成的强连通分量，被依赖的在前面
	adjacency_list calleeVector; //!< 每个图片段内层调用的图片段
	adjacency_list callerVector; //!< 外层调用每个图片段的图片段

private:
	std::vector<MarkedObjectGuard> sectionGuardVector; //!< 保证这些图片段引用的内存不被回收
};

/// 顶点号与具体数据结构相互映射
template<typename T>
struct VertexMapping
{
public:
	/// 根据顶点号获取具体数据结构
	T vertexAt(uint32_t v) const { return vertexVector[v]; }

	/// 根据具体数据结构获取顶点号
	uint32_t vertexId(const T& x) const { return vertexMap.at(x); }

	/// 判断是否存在该顶点
	bool vertexExists(const T& x) const { return vertexMap.count(x) > 0; }

	/// 获取所有顶点
	const std::vector<T>& getVertexVector() const { return vertexVector; };

protected:
	/// 清空
	void clear() { vertexVector.clear(); vertexMap.clear(); }

protected:
	std::vector<T> vertexVector; //!< 顶点号到具体数据结构映射
	std::unordered_map<T, uint32_t> vertexMap; //!< 具体数据结构到顶点号映射
};

/// 子图号与具体数据结构相互映射
template<typename T>
struct SubgraphMapping
{
public:
	/// 根据子图号获取具体数据结构
	T subgraphAt(uint32_t g) const { return subgraphVector[g]; }

	/// 根据具体数据结构获取子图号
	uint32_t subgraphId(const T& x) const { return subgraphMap.at(x); }

	/// 判断是否存在该子图
	bool subgraphExists(const T& x) const { return subgraphMap.count(x) > 0; }

	/// 获取所有子图
	const std::vector<T>& getSubgraphVector() const { return subgraphVector; };

protected:
	/// 清空
	void clear() { subgraphVector.clear(); subgraphMap.clear(); }

	// 重排序
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
	std::vector<T> subgraphVector; //!< 子图号到具体数据结构映射
	std::unordered_map<T, uint32_t> subgraphMap; //!< 具体数据结构到子图号映射
};

/*
* @brief 不保存调用路径的计算图
* @details
* 相同的Section的不同调用点投影到一起
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
	/// 收集图片段作为子图
	virtual void collectSection() = 0;

	/// 收集变量作为顶点
	void collectVariable();

	/// 收集边，并添加常量顶点
	void addEdge(uint32_t vertex, Value* dependency, bool isOperand);
};

/*
* @brief 最外层Section构成的图
* @details
* 每个图片段加载后得到一个子图，子图之间相互独立，没有连通性。
* 内部如果有图片段调用，则视为每个ResultValue生成的顶点，依赖每个实参生成的顶点。
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

	/// 拆分强连通分量中的PhiValue和ResultValue
	VCG_API void splitPhiAndResult(const vertex_list& scc,
		std::vector<PhiValue*>& phiValueVector, std::vector<ResultValue*>& resultValueVector) const;
};

/*
* @brief 按照Value对象对图的顶点进行投影
* @details
* 整个图的子图构成分区关系，形参认为依赖每一个实参
*/
class GraphValueProjection : public GraphNoCallPath
{
public:
	DEFAULT_ALL(GraphValueProjection)
	explicit GraphValueProjection(std::vector<Section*> rootSectionVector) { load(std::move(rootSectionVector)); }

	VCG_API void collectSection() override;
};

/*
* @brief 单个循环以及它的循环体构成的图
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
* @brief 变量按照调用路径展开
* @details
* 由Section调用产生的子图构成嵌套关系
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
	/// 判断是否展开此图片段
	virtual bool expandSection(Section* section) const { return true; }

private:
	/// 收集图片段作为子图，并且收集变量作为顶点
	void collectSectionAndVariable(Section* section, std::vector<SectionCall*>& callVector);

	/// 收集边，并添加常量顶点
	void addEdge(uint32_t vertex, Value* dependency, std::vector<SectionCall*> callVector, bool isOperand);
};
