#pragma once

#ifdef _WIN32

#ifndef BACKEND_EXPORTS
#define BACKEND_API __declspec(dllimport)
#else
#define BACKEND_API __declspec(dllexport)
#endif // !BACKEND_EXPORTS

#else

#define BACKEND_API

#endif // _WIN32
