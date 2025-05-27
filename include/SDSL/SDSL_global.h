#pragma once

// SDSL: Stencil Domain Specific Language（模板计算领域特定语言）

#ifdef _WIN32

#ifndef SDSL_EXPORTS
#define SDSL_API __declspec(dllimport)
#else
#define SDSL_API __declspec(dllexport)
#endif // !SDSL_EXPORTS

#else

#define SDSL_API

#endif // _WIN32
