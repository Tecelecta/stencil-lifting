#pragma once

#include "VCG_global.h"

#include <vector>
#include <cstdint>

typedef std::vector<uint32_t> vertex_list;
typedef std::vector<uint32_t> vertex_mapping;
typedef std::vector<vertex_list> adjacency_list;
typedef std::vector<vertex_list> scc_topological_order;

/// 将顶点排序
VCG_API void sortVertexes(vertex_list& V);

/// 将顶点排序并去重
VCG_API void sortAndUniqueVertexes(vertex_list& V);

/// 将每个顶点的边排序
VCG_API void sortEdges(adjacency_list& G);

/// 将每个顶点的边排序并去重
VCG_API void sortAndUniqueEdges(adjacency_list& G);

/// 计算一个图的反图
VCG_API adjacency_list getReverseGraph(const adjacency_list& G);

/// 计算一个图的导出子图
VCG_API adjacency_list getInducedSubgraph(const adjacency_list& G, const vertex_list& V);

/// 记录可达的子图
VCG_API std::vector<bool> visitReachableSubGraph(const adjacency_list& G, const vertex_list& V);

/// 打包可达的子图
VCG_API vertex_list packReachableSubGraph(const adjacency_list& G, const vertex_list& V);

/// 计算一个图的强连通分量
VCG_API scc_topological_order computeSCC(const adjacency_list& G);

/// 给出反图的情况下，计算一个图的强连通分量
VCG_API scc_topological_order computeSCC(const adjacency_list& G, const adjacency_list& rG);

/// 判断一个强连通分量是否平凡
VCG_API bool isTrivialSCC(const vertex_list& scc, const adjacency_list& G);

/// 根据顶点的映射关系，对边进行变换
VCG_API adjacency_list transformEdgesByMapping(const adjacency_list& G, const vertex_mapping& f);
