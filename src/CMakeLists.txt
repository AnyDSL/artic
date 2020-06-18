add_library(libartic
    ast.h
    ast.cpp
    bind.cpp
    bind.h
    cast.h
    check.cpp
    check.h
    emit.cpp
    emit.h
    lexer.cpp
    lexer.h
    loc.h
    locator.h
    log.h
    log.cpp
    parser.cpp
    parser.h
    print.cpp
    print.h
    symbol.h
    token.h
    types.cpp
    types.h)

set_target_properties(libartic PROPERTIES PREFIX "" CXX_STANDARD 17)

find_package(Thorin REQUIRED)
target_link_libraries(libartic PUBLIC ${Thorin_LIBRARIES})
target_include_directories(libartic PUBLIC ${Thorin_INCLUDE_DIRS})

add_executable(artic main.cpp ${BACKEND})
set_target_properties(artic PROPERTIES CXX_STANDARD 17)
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_link_libraries(artic libartic)

if (${COLORIZE})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
