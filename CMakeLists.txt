cmake_minimum_required(VERSION 3.4.3 FATAL_ERROR)
project(artic)

set(PROJECT_VERSION_MAJOR 0)
set(PROJECT_VERSION_MINOR 2)
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "limited config" FORCE)

option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(CODE_COVERAGE "Enable code coverage using gcov in Debug builds" OFF)

if (CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Debug or Release" FORCE)
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Compile for code coverage in Debug mode
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
if (CODE_COVERAGE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
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

add_subdirectory(src)
if (BUILD_TESTING)
    include(CTest)
    add_subdirectory(test)
endif ()

function (cat IN_FILE OUT_FILE)
    file(READ ${IN_FILE} CONTENTS)
    file(APPEND ${OUT_FILE} "${CONTENTS}")
endfunction()

file(GLOB STDLIB "./libraries/*.art")
file(WRITE "./bin/stdlib.art")

foreach(LIB ${STDLIB})
    cat(${LIB} ./bin/stdlib.art)
endforeach()

export(TARGETS libartic artic FILE ${CMAKE_BINARY_DIR}/share/anydsl/cmake/artic-exports.cmake)
configure_file(cmake/artic-config.cmake.in ${CMAKE_BINARY_DIR}/share/anydsl/cmake/artic-config.cmake @ONLY)
