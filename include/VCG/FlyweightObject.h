#pragma once

#include "Hash.h"
#include "MarkedObject.h"

/**
* @brief 用于支持享元模式的基类
* @details
* 享元类应当继承该基类，然后再让它的实现类分别继承该类的实现类和工厂类。
* 该基类提供享元类需要的基本方法，使得子类不需要再复制相似代码。
* 该类和所有子类是值类型，作为函数参数和返回值时应使用值传递。
* 公有构造函数只能构造空对象，表示一个无效的值；或者拷贝已有的对象。
* 需要使用 TypeManager 类创建新对象。
*/
class FlyweightObject
{
protected:
	/**
	* @brief 享元类的内部实现的基类
	* @see FlyweightObject
	*/
	class Impl : public MarkedObject
	{
	public:
		explicit Impl(Context* context) : MarkedObject(context) {}

	protected:
		~Impl() override = default;

	public:
		size_t hashCode = 0; //!< 保存享元对象的哈希值，此值应在子类构造函数中计算
	};

	/**
	* @brief 获取它的内部实现
	* @details
	* 该类应仅被享元类对应工厂类使用。
	* 子类的实现类如果继承了基类的实现类，则应当隐藏该方法，重新实现一个返回子类实现类的同名方法。
	*/
	const Impl& getImpl() const { return *impl_ptr; }

public:
	FlyweightObject(Impl* impl_ptr = nullptr) : impl_ptr(impl_ptr) {}

	/// 获取该对象所属的上下文
	Context* getContext() const { return getImpl().getContext(); }

	/// 获取对象的哈希值
	size_t hash() const { return impl_ptr == nullptr ? 0 : impl_ptr->hashCode; }

	/// 判断是否与另一个实例表示同一个数据
	bool operator==(const FlyweightObject& other) const { return impl_ptr == other.impl_ptr; }

	/// 判断是否与另一个实例表示不同的数据
	bool operator!=(const FlyweightObject& other) const { return impl_ptr != other.impl_ptr; }

	/// 判断是否为空对象
	bool operator==(nullptr_t) const { return impl_ptr == nullptr; }

	/// 判断是否为非空对象
	bool operator!=(nullptr_t) const { return impl_ptr != nullptr; }

	/**
	* @brief 标记一个需要GC的享元对象，在重写 markReference 方法中使用
	* @see MarkedObject::markObject
	*/
	void markImpl() { if (impl_ptr != nullptr) impl_ptr->markSelf(); }

	/// 检查是否被标记
	bool checkMarked() const { return getImpl().checkMarked(); }

protected:
	Impl* impl_ptr = nullptr;
	friend class FlyweightObjectGuard;

public:
	/**
	* @brief 用于创建实例的工厂类的基类
	* @details
	* 对于每种不同的享元类，每个上下文应拥有一个工厂类的实例，统一使用它来创建该上下文内的享元类对象。
	* @see FlyweightObject
	* @see Context
	*/
	class Factory
	{
	protected:
		explicit Factory(Context* context) : context(context) {}
		DISABLE_COPY_MOVE(Factory)

		virtual ~Factory() = default;

		/**
		* @brief 回收工厂内部对象池中无用的对象
		* @details
		* 该方法仅在运行“标记-删除”算法时使用，否则是没有意义的。
		* 子类应实现该方法，移除对象池中未被标记的对象，但是不要delete内存。
		* @see GarbageCollector
		*/
		virtual void eraseUseless() = 0;

	protected:
		Context* context;
	};
};

/**
* @brief 用于保证有用的对象不被垃圾回收器回收
* @details
* 使用方法：将需要保护的根对象传入构造函数的参数
* 销毁上下文之前需要先销毁守卫对象
*/
class FlyweightObjectGuard final
{
public:
	VCG_API explicit FlyweightObjectGuard(FlyweightObject object = FlyweightObject()) : guard(object.impl_ptr) {}
	DEFAULT_COPY_MOVE(FlyweightObjectGuard)

	FlyweightObject getObject() const { return dynamic_cast<FlyweightObject::Impl*>(guard.getObject()); }

private:
	MarkedObjectGuard guard;
};

STD_HASH(FlyweightObject)
