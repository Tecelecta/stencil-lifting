#pragma once

#define DISABLE_COPY(Class) \
	Class(const Class &) = delete;\
	Class &operator=(const Class &) = delete;

#define DISABLE_MOVE(Class) \
	Class(Class &&) = delete; \
	Class &operator=(Class &&) = delete;

#define DISABLE_COPY_MOVE(Class) \
	DISABLE_COPY(Class) \
	DISABLE_MOVE(Class)

#define DEFAULT_COPY(Class) \
	Class(const Class &) = default;\
	Class &operator=(const Class &) = default;

#define DEFAULT_MOVE(Class) \
	Class(Class &&) = default; \
	Class &operator=(Class &&) = default;

#define DEFAULT_COPY_MOVE(Class) \
	DEFAULT_COPY(Class) \
	DEFAULT_MOVE(Class)

#define DEFAULT_ALL(Class) \
	Class() = default; \
	DEFAULT_COPY_MOVE(Class)
