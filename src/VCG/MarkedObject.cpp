#define VCG_EXPORTS

#include "Context.h"

MarkedObject::MarkedObject(Context* context)
	: context(context)
{
	if (context == nullptr)
	{
		throw NullContextError();
	}
	context->addObject(this);
}

MarkedObject::MarkedObject(const MarkedObject& src)
	: context(src.context)
{
	context->addObject(this);
}

MarkedObject& MarkedObject::operator=(const MarkedObject& src)
{
	if (this != &src)
	{
		context = src.context;
		context->addObject(this);
	}
	return *this;
}

void MarkedObject::markSelf()
{
	if (this != nullptr)
	{
		context->markObject(this);
	}
}

MarkedObjectGuard::MarkedObjectGuard(MarkedObject* object) : object(object)
{
	if (object != nullptr)
	{
		object->rootRef = 1;
		object->getContext()->rootUsefulSet.insert(object);
	}
}

MarkedObjectGuard::MarkedObjectGuard(const MarkedObjectGuard& src) : object(src.object)
{
	object->rootRef++;
}

MarkedObjectGuard& MarkedObjectGuard::operator=(const MarkedObjectGuard& src)
{
	if (this != &src)
	{
		object = src.object;
		object->rootRef++;
	}
	return *this;
}

MarkedObjectGuard::MarkedObjectGuard(MarkedObjectGuard&& src) noexcept : object(src.object)
{
	src.object = nullptr;
}

MarkedObjectGuard& MarkedObjectGuard::operator=(MarkedObjectGuard&& src) noexcept
{
	if (this != &src)
	{
		object = src.object;
		src.object = nullptr;
	}
	return *this;
}

MarkedObjectGuard::~MarkedObjectGuard()
{
	if (object != nullptr && --object->rootRef == 0)
	{
		object->getContext()->rootUsefulSet.erase(object);
	}
}
