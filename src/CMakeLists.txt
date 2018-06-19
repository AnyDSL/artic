set(BACKEND emit.cpp)
find_package(Thorin QUIET)
if (Thorin_FOUND)
    set(BACKEND emit_thorin.cpp)
    include_directories(${Thorin_INCLUDE_DIRS})
    link_libraries(${Thorin_LIBRARIES})
endif()

add_library(libartic
    ast.h
    ast.cpp
    bind.cpp
    bind.h
    box.h
    cast.h
    check.cpp
    check.h
    ${BACKEND}
    infer.cpp
    infer.h
    lexer.cpp
    lexer.h
    loc.h
    parser.cpp
    parser.h
    print.cpp
    print.h
    symbol.h
    token.h
    type.cpp
    type.h)
set_target_properties(libartic PROPERTIES PREFIX "")

add_executable(artic main.cpp)
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR})
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_link_libraries(artic libartic)

if (${COLORIZE_OUTPUT})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
