add_executable(artic
    main.cpp
    ast.h
    ast.cpp
    bind_names.cpp
    bind_names.h
    box.h
    cast.h
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
    type.h
    type_check.cpp
    type_check.h)

if (${COLORIZE_OUTPUT})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
