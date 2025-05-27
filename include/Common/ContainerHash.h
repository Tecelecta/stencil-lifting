#pragma once

#include <vector>

template<typename T1, typename T2>
struct std::hash<std::pair<T1, T2>>
{
	size_t operator()(const std::pair<T1, T2>& p) const
	{
		return std::hash<T1>()(p.first) ^ std::hash<T2>()(p.second);
	}
};

template<typename T>
struct std::hash<std::vector<T>>
{
	size_t operator()(const std::vector<T>& v) const
	{
		size_t result = 0;
		for (const T& x : v)
		{
			result ^= std::hash<T>()(x);
		}
		return result + v.size();
	}
};
