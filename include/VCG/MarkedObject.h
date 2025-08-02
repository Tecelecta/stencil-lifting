#pragma once

#include "Constructor.h"
#include "VCG_global.h"

#include <stdexcept>

class Context;

/**
* @brief 
* @details
* 
* -GC
*  markReference 
* @see GarbageCollector
*/
class MarkedObject
{
public:
	/// 
	Context* getContext() const { return context; }

	class NullContextError : public std::invalid_argument
	{
	public:
		NullContextError() : std::invalid_argument("Using null context to build an object!") {}
	};

protected:
	VCG_API explicit MarkedObject(Context* context); // 
	VCG_API MarkedObject(const MarkedObject& src);
	VCG_API MarkedObject& operator=(const MarkedObject& src);
	DISABLE_MOVE(MarkedObject)
	virtual ~MarkedObject() = default;

	/**
	* @brief GC
	* @details
	*  markReference 
	* 
	* 
	* 
	*/
	virtual void markReference() {}

public:
	/// GC markReference 
	VCG_API void markSelf();

	/// 
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
* @brief 
* @details
* 
* 
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
	MarkedObject* object; //!< 
};
