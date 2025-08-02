#pragma once

#include "Hash.h"
#include "MarkedObject.h"

/**
* @brief 
* @details
* 
* 
* 
* 
*  TypeManager 
*/
class FlyweightObject
{
protected:
	/**
	* @brief 
	* @see FlyweightObject
	*/
	class Impl : public MarkedObject
	{
	public:
		explicit Impl(Context* context) : MarkedObject(context) {}

	protected:
		~Impl() override = default;

	public:
		size_t hashCode = 0; //!< 
	};

	/**
	* @brief 
	* @details
	* 
	* 
	*/
	const Impl& getImpl() const { return *impl_ptr; }

public:
	FlyweightObject(Impl* impl_ptr = nullptr) : impl_ptr(impl_ptr) {}

	/// 
	Context* getContext() const { return getImpl().getContext(); }

	/// 
	size_t hash() const { return impl_ptr == nullptr ? 0 : impl_ptr->hashCode; }

	/// 
	bool operator==(const FlyweightObject& other) const { return impl_ptr == other.impl_ptr; }

	/// 
	bool operator!=(const FlyweightObject& other) const { return impl_ptr != other.impl_ptr; }

	/// 
	bool operator==(nullptr_t) const { return impl_ptr == nullptr; }

	/// 
	bool operator!=(nullptr_t) const { return impl_ptr != nullptr; }

	/**
	* @brief GC markReference 
	* @see MarkedObject::markObject
	*/
	void markImpl() { if (impl_ptr != nullptr) impl_ptr->markSelf(); }

	/// 
	bool checkMarked() const { return getImpl().checkMarked(); }

protected:
	Impl* impl_ptr = nullptr;
	friend class FlyweightObjectGuard;

public:
	/**
	* @brief 
	* @details
	* 
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
		* @brief 
		* @details
		* -
		* delete
		* @see GarbageCollector
		*/
		virtual void eraseUseless() = 0;

	protected:
		Context* context;
	};
};

/**
* @brief 
* @details
* 
* 
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
