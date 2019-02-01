cmake_minimum_required(VERSION 3.1)
project(artic)

set(PROJECT_VERSION_MAJOR 0)
set(PROJECT_VERSION_MINOR 1)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

include_directories(contrib/half/include)
include_directories(contrib/utf8-cpp/source)

set(COLOR_TTY_AVAILABLE TRUE)
if (WIN32)
    # By default, Windows console does not support ANSI escape codes
    set(COLOR_TTY_AVAILABLE FALSE)
endif ()
set(COLORIZE ${COLOR_TTY_AVAILABLE} CACHE BOOL "Set to TRUE to enable colorized output. Requires an ANSI compliant terminal.")
set(NO_BACKEND FALSE CACHE BOOL "Disable the backend to speed up compilation and debugging.")

add_subdirectory(src)
