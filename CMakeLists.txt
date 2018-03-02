cmake_minimum_required(VERSION 3.1)
project(artic)

set(CMAKE_CXX_STANDARD 14)

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
set(COLORIZE_OUTPUT ${COLOR_TTY_AVAILABLE} CACHE BOOL "Set to TRUE to enable colorized output. Requires an ANSI compliant terminal.")

add_subdirectory(src)
