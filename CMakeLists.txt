cmake_minimum_required(VERSION 3.16)
cmake_policy(SET CMP0003 NEW)
cmake_policy(SET CMP0167 NEW)
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()


project(Stencil CXX)

find_package(Z3 4.12 REQUIRED)
find_package(Z3 REQUIRED)
find_package(Boost REQUIRED)

message(STATUS "found z3 in ${PACKAGE_PREFIX_DIR}")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED true)

if (MSVC)
	set(CMAKE_CXX_FLAGS_DEBUG /Od)
	set(CMAKE_CXX_FLASG_RELEASE /O2)
else()
	set(CMAKE_CXX_FLAGS_DEBUG -O0)
	set(CMAKE_CXX_FLAGS_RELEASE -O2)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	endif()
endif()

include_directories ("${PROJECT_SOURCE_DIR}/include")
include_directories ("${PROJECT_SOURCE_DIR}/include/Common")
include_directories (${Boost_INCLUDE_DIRS})

file(GLOB SDSL_SRC ${PROJECT_SOURCE_DIR}/src/SDSL/*.cpp)
file(GLOB Backend_SRC ${PROJECT_SOURCE_DIR}/src/Backend/*.cpp)
file(GLOB VCG_SRC ${PROJECT_SOURCE_DIR}/src/VCG/*.cpp)
file(GLOB SDSLC_SRC ${PROJECT_SOURCE_DIR}/src/SDSLC/*.cpp)

add_library (SDSL OBJECT ${SDSL_SRC})
target_include_directories (SDSL PUBLIC "${PROJECT_SOURCE_DIR}/include/SDSL")

add_library(Backend OBJECT ${Backend_SRC})
target_include_directories (Backend PUBLIC "${PROJECT_SOURCE_DIR}/include/Backend" ${Z3_CXX_INCLUDE_DIRS})

add_library(VCG OBJECT ${VCG_SRC})
target_include_directories (VCG PUBLIC "${PROJECT_SOURCE_DIR}/include/VCG")
target_link_libraries (VCG PUBLIC ${Z3_LIBRARIES})

add_executable (sdslc ${SDSLC_SRC})
target_include_directories (sdslc PUBLIC "${PROJECT_SOURCE_DIR}/include/SDSLC")
target_link_libraries (sdslc PUBLIC SDSL Backend VCG)
