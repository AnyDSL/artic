add_library(libartic
    ast.h
    ast.cpp
    bind.cpp
    bind.h
    box.h
    cast.h
    check.cpp
    check.h
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
target_link_libraries(artic libartic)

if (${COLORIZE_OUTPUT})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
