#define VCG_EXPORTS

#include "AnyString.h"

String::Impl::Impl(Context* context, std::string_view data)
	: Symbol::Impl(context), data(data)
{
	this->data.shrink_to_fit();
	hashCode = std::hash<std::string>()(this->data);
}

void String::Factory::eraseUseless()
{
	for (auto iter = objectPool.begin(); iter != objectPool.end();)
	{
		if (!iter->second.checkMarked())
		{
			objectPool.erase(iter++);
			continue;
		}
		iter++;
	}
}

String String::Factory::getString(std::string_view data)
{
	auto iter = objectPool.find(data);
	if (iter == objectPool.end())
	{
		auto impl_ptr = new String::Impl(context, data);
		iter = objectPool.emplace(impl_ptr->data, impl_ptr).first;
	}
	return iter->second;
}
