find_package(Thorin)

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
    world.cpp
    world.h)

set_target_properties(libartic PROPERTIES PREFIX "" CXX_STANDARD 17)
target_include_directories(libartic PUBLIC ${Thorin_INCLUDE_DIRS})
target_link_libraries(libartic ${Thorin_LIBRARIES})

add_executable(artic main.cpp ${BACKEND})
set_target_properties(artic PROPERTIES CXX_STANDARD 17)
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_include_directories(artic PUBLIC ${Thorin_INCLUDE_DIRS})
target_link_libraries(artic libartic ${Thorin_LIBRARIES})

if (${COLORIZE})
    target_compile_definitions(artic PUBLIC -DCOLORIZE)
endif()
