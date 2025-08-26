#pragma once

#include "Graph.h"

/// 
template<typename GraphType>
class Pass : public GraphType
{
protected:
	Pass() = default;
	DISABLE_COPY_MOVE(Pass)

public:
	/// load
	void setGraph(GraphType graph)
	{
		static_cast<GraphType&>(*this) = std::move(graph);
	}

public:
	std::vector<bool> vertexFilter;
	std::vector<bool> subgraphFilter;
	std::vector<uint32_t> vertexEquClass; //! 
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
