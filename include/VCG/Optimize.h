#pragma once

#include "Graph.h"

VCG_API GraphValueProjection runOptimizeLevel1(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols);

VCG_API GraphValueProjection runOptimizeLevel2(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols);

VCG_API GraphValueProjection runOptimizeLevel3(GraphValueProjection graph, const std::unordered_set<String>& initUsefulSymbols);
