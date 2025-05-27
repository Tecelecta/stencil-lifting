#pragma once

#include "MarkedObject.h"

#include <vector>
#include <queue>
#include <unordered_set>

/**
* @brief 用于支持基于垃圾收集的内存管理机制的管理类
* @details
* 是上下文组成部分，管理该上下文内的所有需要垃圾回收机制的对象。
* @see MarkedObject
* @see Context
*/
class GarbageCollector
{
protected:
	GarbageCollector(Context* context) : context(context) {}
	DISABLE_COPY_MOVE(GarbageCollector)

public:
	/// 清空对象池，删除所有对象
	VCG_API void deleteAll();

	/// 运行“标记-删除”算法，删除未被引用的对象
	VCG_API void deleteUseless();

private:
	void addObject(MarkedObject* ptr);

	void markObject(MarkedObject* object);

protected:
	std::unordered_set<MarkedObject*> rootUsefulSet; /// 需要保留的根对象，该集合内的对象和它们的引用不被删除

private:
	Context* context;
	std::vector<MarkedObject*> objectPool; //!< 保存所有对象
	std::queue<MarkedObject*> toSearch; //!< 广度优先搜索用的队列
	friend MarkedObject;
	friend MarkedObjectGuard;
};
