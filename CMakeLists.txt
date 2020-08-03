cmake_minimum_required(VERSION 3.1)
project(artic)

set(PROJECT_VERSION_MAJOR 0)
set(PROJECT_VERSION_MINOR 1)

option(BUILD_SHARED_LIBS "Build shared libraries" ON)

if(CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Debug or Release" FORCE)
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Compile for code coverage in Debug mode
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    include(CodeCoverage)
    append_coverage_compiler_flags()
endif()

set(COLOR_TTY_AVAILABLE TRUE)
if (WIN32)
    # By default, Windows console does not support ANSI escape codes
    set(COLOR_TTY_AVAILABLE FALSE)
endif ()
set(COLORIZE ${COLOR_TTY_AVAILABLE} CACHE BOOL "Set to TRUE to enable colorized output. Requires an ANSI compliant terminal.")

find_package(Thorin REQUIRED)

include(CTest)
add_subdirectory(src)
add_subdirectory(test)
