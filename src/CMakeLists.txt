add_executable(artic
    main.cpp
    ast.h
    ast.cpp
    box.h
    cast.h
    lexer.cpp
    lexer.h
    loc.h
    parser.cpp
    parser.h
    print.cpp
    print.h
    token.h
    type.cpp
    type.h)

if (${COLORIZE_OUTPUT})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
