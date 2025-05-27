#pragma once

#include "Constructor.h"
#include "VCG_global.h"

#include <stdexcept>

class Context;

/**
* @brief 用于支持基于垃圾收集的内存管理机制的基类
* @details
* 对于生命周期不易确定，或循环引用较复杂的对象，本项目使用垃圾收集机制。
* 垃圾收集算法使用“标记-删除”算法，此类是需要支持GC机制的类的基类。
* 如果此类引用了其他需要被标记的对象，则应重写 markReference 方法，否则造成内存泄露。
* @see GarbageCollector
*/
class MarkedObject
{
public:
	/// 获取该对象所属的上下文
	Context* getContext() const { return context; }

	class NullContextError : public std::invalid_argument
	{
	public:
		NullContextError() : std::invalid_argument("Using null context to build an object!") {}
	};

protected:
	VCG_API explicit MarkedObject(Context* context); // 每个被标记的对象需要被一个上下文管理
	VCG_API MarkedObject(const MarkedObject& src);
	VCG_API MarkedObject& operator=(const MarkedObject& src);
	DISABLE_MOVE(MarkedObject)
	virtual ~MarkedObject() = default;

	/**
	* @brief 标记它引用的需要GC的对象
	* @details
	* 如果此类引用了其他需要被标记的对象，则应重写 markReference 方法，否则造成内存错误。
	* 建议不要在最终子类中标记子类和所有基类引用的其他对象；
	* 而应该在子类调用的基类的该方法，每个类中标记自身的引用；
	* 如果基类确定不会引用其他对象，则子类可以不调用基类的该方法。
	*/
	virtual void markReference() {}

public:
	/// 标记一个需要GC的对象，在重写 markReference 方法中使用
	VCG_API void markSelf();

	/// 检查是否被标记
	bool checkMarked() const { return isMarked; }

private:
	Context* context;
	uint32_t rootRef = 0;
	bool isMarked = true;
	friend class GarbageCollector;
	friend class MarkedObjectGuard;
	friend class FlyweightObject;
	friend class FlyweightObjectGuard;
};

/**
* @brief 用于保证有用的对象不被垃圾回收器回收
* @details
* 使用方法：将需要保护的根对象传入构造函数的参数
* 销毁上下文之前需要先销毁守卫对象
*/
class MarkedObjectGuard final
{
public:
	VCG_API explicit MarkedObjectGuard(MarkedObject* object = nullptr);
	VCG_API MarkedObjectGuard(const MarkedObjectGuard& src);
	VCG_API MarkedObjectGuard& operator=(const MarkedObjectGuard& src);
	VCG_API MarkedObjectGuard(MarkedObjectGuard&& src) noexcept;
	VCG_API MarkedObjectGuard& operator=(MarkedObjectGuard&& src) noexcept;
	VCG_API ~MarkedObjectGuard();

	MarkedObject* getObject() const { return object; }

private:
	MarkedObject* object; //!< 被保护的对象
};
