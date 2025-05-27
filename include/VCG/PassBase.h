#pragma once

#include "Graph.h"

/// 所有遍的模板基类
template<typename GraphType>
class Pass : public GraphType
{
protected:
	Pass() = default;
	DISABLE_COPY_MOVE(Pass)

public:
	/// 直接使用一个现有的图，相比load方法更简便
	void setGraph(GraphType graph)
	{
		static_cast<GraphType&>(*this) = std::move(graph);
	}

public:
	std::vector<bool> vertexFilter;
	std::vector<bool> subgraphFilter;
	std::vector<uint32_t> vertexEquClass; //! 顶点属于哪个等价类，并查集数据结构，值等于下标的是根
};

class TransformResult
{
public:
	virtual Value* mapValue(Value* value) const = 0;

	virtual Section* mapSection(Section* section) const = 0;

public:
	const TransformResult* parent = nullptr;
	std::vector<Value*> valueResultVector;
	std::vector<Section*> sectionResultVector;
};
