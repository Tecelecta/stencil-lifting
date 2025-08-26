#pragma once

#include <cstddef>
#include <type_traits>
#include <functional>

#define STD_HASH(T) \
	template<> struct std::hash<T> { size_t operator()(const T& src) const { return src.hash(); } };

/// 
template<typename T>
struct PointerHasher
{
	size_t operator()(const T* a) const { return std::hash<T>()(*a); }
};

/// 
template<typename T>
struct PointerEqualTo
{
	bool operator()(const T* a, const T* b) const { return a == b || *a == *b; }
};
