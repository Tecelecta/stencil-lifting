#define VCG_EXPORTS

#include "GarbageCollector.h"

#include <cassert>

void GarbageCollector::deleteAll()
{
	for (auto object : objectPool)
	{
		delete object;
	}
	objectPool.clear();
}

void GarbageCollector::deleteUseless()
{
	for (auto& object : objectPool)
	{
		object->isMarked = false;
	}
	for (auto rootObject : rootUsefulSet)
	{
		assert(toSearch.empty());
		rootObject->isMarked = true;
		toSearch.push(rootObject);
		do {
			assert(toSearch.front()->isMarked);
			toSearch.front()->markReference();
			toSearch.pop();
		} while (!toSearch.empty());
	}
	std::vector<MarkedObject*> newPool;
	for (auto& object : objectPool)
	{
		if (object->isMarked)
		{
			newPool.push_back(object);
		}
		else
		{
			delete object;
		}
	}
	objectPool.swap(newPool);
}

void GarbageCollector::addObject(MarkedObject* ptr)
{
	objectPool.emplace_back(ptr);
}

void GarbageCollector::markObject(MarkedObject* object)
{
	if (!object->isMarked)
	{
		object->isMarked = true;
		toSearch.push(object);
	}
}
