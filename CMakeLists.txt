cmake_minimum_required(VERSION 3.1)
project(artic)

set(CMAKE_CXX_STANDARD 11)

include_directories(contrib/half/include)
include_directories(contrib/utf8-cpp/source)

add_subdirectory(src)
