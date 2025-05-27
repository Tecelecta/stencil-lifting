#pragma once

// VCG: Verifiable Computational Graph（可验证的计算图）

#ifdef _WIN32

#ifndef VCG_EXPORTS
#define VCG_API __declspec(dllimport)
#else
#define VCG_API __declspec(dllexport)
#endif // !VCG_EXPORTS

#else

#define VCG_API

#endif // _WIN32

#define DECL_HASH_EQ_NE_API(T) \
	VCG_API size_t hash() const; \
	VCG_API bool operator==(const T& other) const; \
	VCG_API bool operator!=(const T& other) const;

#define BOOST_PFR_HASH_EQ_NE_API(T) \
	size_t T::hash() const { return boost::pfr::hash_fields(*this); } \
	bool T::operator==(const T& other) const { return boost::pfr::eq_fields(*this, other); } \
	bool T::operator!=(const T& other) const { return boost::pfr::ne_fields(*this, other); }
