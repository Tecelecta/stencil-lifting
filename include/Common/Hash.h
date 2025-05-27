#pragma once

#include <cstddef>
#include <type_traits>
#include <functional>

#define STD_HASH(T) \
	template<> struct std::hash<T> { size_t operator()(const T& src) const { return src.hash(); } };

/// 以被指向的对象的哈希函数作为指针的哈希函数
template<typename T>
struct PointerHasher
{
	size_t operator()(const T* a) const { return std::hash<T>()(*a); }
};

/// 以被指向的对象是否相等来作为指针是否相等判断条件
template<typename T>
struct PointerEqualTo
{
	bool operator()(const T* a, const T* b) const { return a == b || *a == *b; }
};
