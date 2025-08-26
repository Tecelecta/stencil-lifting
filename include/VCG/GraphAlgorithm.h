#pragma once

#include "VCG_global.h"

#include <vector>
#include <cstdint>

typedef std::vector<uint32_t> vertex_list;
typedef std::vector<uint32_t> vertex_mapping;
typedef std::vector<vertex_list> adjacency_list;
typedef std::vector<vertex_list> scc_topological_order;

/// 
VCG_API void sortVertexes(vertex_list& V);

/// 
VCG_API void sortAndUniqueVertexes(vertex_list& V);

/// 
VCG_API void sortEdges(adjacency_list& G);

/// 
VCG_API void sortAndUniqueEdges(adjacency_list& G);

/// 
VCG_API adjacency_list getReverseGraph(const adjacency_list& G);

/// 
VCG_API adjacency_list getInducedSubgraph(const adjacency_list& G, const vertex_list& V);

/// 
VCG_API std::vector<bool> visitReachableSubGraph(const adjacency_list& G, const vertex_list& V);

/// 
VCG_API vertex_list packReachableSubGraph(const adjacency_list& G, const vertex_list& V);

/// 
VCG_API scc_topological_order computeSCC(const adjacency_list& G);

/// 
VCG_API scc_topological_order computeSCC(const adjacency_list& G, const adjacency_list& rG);

/// 
VCG_API bool isTrivialSCC(const vertex_list& scc, const adjacency_list& G);

/// 
VCG_API adjacency_list transformEdgesByMapping(const adjacency_list& G, const vertex_mapping& f);
