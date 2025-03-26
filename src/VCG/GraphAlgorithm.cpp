#define VCG_EXPORTS

#include "GraphAlgorithm.h"

#include <cassert>
#include <algorithm>
#include <queue>

void sortVertexes(vertex_list& V)
{
	std::sort(V.begin(), V.end());
}

void sortAndUniqueVertexes(vertex_list& V)
{
	sortVertexes(V);
	V.erase(std::unique(V.begin(), V.end()), V.end());
}

void sortEdges(adjacency_list& G)
{
	for (auto& V : G)
	{
		sortVertexes(V);
	}
}

void sortAndUniqueEdges(adjacency_list& G)
{
	for (auto& V : G)
	{
		sortAndUniqueVertexes(V);
	}
}

adjacency_list getReverseGraph(const adjacency_list& G)
{
	adjacency_list rG(G.size());
	for (uint32_t i = 0; i < G.size(); i++)
	{
		for (uint32_t j : G[i])
		{
			rG[j].push_back(i);
		}
	}
	return rG;
}

adjacency_list getInducedSubgraph(const adjacency_list& G, const vertex_list& V)
{
	std::vector<bool> selected(G.size(), false);
	std::vector<uint32_t> f(G.size());
	for (uint32_t i = 0; i < V.size(); i++)
	{
		uint32_t v = V[i];
		selected[v] = true;
		f[v] = i;
	}
	adjacency_list result(V.size());
	for (uint32_t i = 0; i < V.size(); i++)
	{
		uint32_t v = V[i];
		for (uint32_t j : G[v])
		{
			if (selected[j])
			{
				result[i].push_back(f[j]);
			}
		}
	}
	return result;
}

std::vector<bool> visitReachableSubGraph(const adjacency_list& G, const vertex_list& V)
{
	std::queue<uint32_t> Q;
	std::vector<bool> visited(G.size(), false);
	for (uint32_t v : V)
	{
		Q.push(v);
		visited[v] = true;
	}
	while (!Q.empty())
	{
		size_t i = Q.front();
		Q.pop();
		for (auto j : G[i])
		{
			if (!visited[j])
			{
				Q.push(j);
				visited[j] = true;
			}
		}
	}
	return visited;
}

vertex_list packReachableSubGraph(const adjacency_list& G, const vertex_list& V)
{
	auto visited = visitReachableSubGraph(G, V);
	std::vector<uint32_t> result;
	for (uint32_t i = 0; i < visited.size(); i++)
	{
		if (visited[i])
		{
			result.push_back(i);
		}
	}
	return result;
}

/// 此算法可以同时求出强连通分量和拓扑序
class Kosaraju
{
public:
	Kosaraju(const adjacency_list& G, const adjacency_list& rG)
		: G(G), rG(rG) {}

private:
	void dfs1(uint32_t v)
	{
		visited[v] = true;
		for (auto iter = G[v].rbegin(); iter != G[v].rend(); ++iter)
		{
			uint32_t i = *iter;
			if (!visited[i])
			{
				dfs1(i);
			}
		}
		S.push_back(v);
	}

	void dfs2(uint32_t v)
	{
		visited[v] = true;
		for (auto iter = rG[v].rbegin(); iter != rG[v].rend(); ++iter)
		{
			uint32_t i = *iter;
			if (!visited[i])
			{
				dfs2(i);
			}
		}
		sccVector.back().push_back(v);
	}

public:
	void run()
	{
		S.reserve(G.size());
		visited = std::vector<bool>(G.size(), false);
		for (uint32_t i = (uint32_t)G.size() -  1; i > 0; i--)
		{
			if (!visited[i])
			{
				dfs1(i);
			}
		}
		if (!visited[0])
		{
			dfs1(0);
		}
		visited = std::vector<bool>(G.size(), false);
		while (!S.empty())
		{
			auto v = S.back();
			S.pop_back();
			if (!visited[v])
			{
				sccVector.emplace_back();
				dfs2(v);
			}
		}
	}

private:
	const adjacency_list& G;
	const adjacency_list& rG;
	std::vector<bool> visited;
	std::vector<uint32_t> S;

public:
	scc_topological_order sccVector;
};

scc_topological_order computeSCC(const adjacency_list& G)
{
	return computeSCC(G, getReverseGraph(G));
}

scc_topological_order computeSCC(const adjacency_list& G, const adjacency_list& rG)
{
	assert(G.size() == rG.size());
	if (G.empty())
	{
		return {};
	}
	Kosaraju kosaraju(G, rG);
	kosaraju.run();
	for (auto& scc : kosaraju.sccVector)
	{
		sortVertexes(scc);
	}
	return std::move(kosaraju.sccVector);
}

bool isTrivialSCC(const vertex_list& scc, const adjacency_list& G)
{
	assert(!scc.empty());
	if (scc.size() == 1)
	{
		auto v = scc.front();
		if (std::find(G[v].begin(), G[v].end(), v) == G[v].end())
		{
			// 平凡强连通分量：只有一个顶点并且不含自环
			return true;
		}
	}
	return false;
}

adjacency_list transformEdgesByMapping(const adjacency_list& G, const vertex_mapping& f)
{
	assert(G.size() == f.size());
	adjacency_list result(*std::max_element(f.begin(), f.end()) + 1);
	for (uint32_t i = 0; i < f.size(); i++)
	{
		result[f[i]].resize(G[i].size());
		for (uint32_t j = 0; j < G[i].size(); j++)
		{
			result[f[i]][j] = f[G[i][j]];
		}
	}
	sortAndUniqueEdges(result);
	return result;
}
