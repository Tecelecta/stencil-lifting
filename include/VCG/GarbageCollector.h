#pragma once

#include "MarkedObject.h"

#include <vector>
#include <queue>
#include <unordered_set>

/**
* @brief 
* @details
* 
* @see MarkedObject
* @see Context
*/
class GarbageCollector
{
protected:
	GarbageCollector(Context* context) : context(context) {}
	DISABLE_COPY_MOVE(GarbageCollector)

public:
	/// 
	VCG_API void deleteAll();

	/// -
	VCG_API void deleteUseless();

private:
	void addObject(MarkedObject* ptr);

	void markObject(MarkedObject* object);

protected:
	std::unordered_set<MarkedObject*> rootUsefulSet; /// 

private:
	Context* context;
	std::vector<MarkedObject*> objectPool; //!< 
	std::queue<MarkedObject*> toSearch; //!< 
	friend MarkedObject;
	friend MarkedObjectGuard;
};
