add_library(libartic
    ir/node.cpp
    ir/module.cpp
    ast.cpp
    bind.cpp
    check.cpp
    emit.cpp
    lexer.cpp
    log.cpp
    parser.cpp
    print.cpp
    types.cpp)

set_target_properties(libartic PROPERTIES PREFIX "" CXX_STANDARD 17)
target_include_directories(libartic PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
if (${COLORIZE})
    target_compile_definitions(libartic PRIVATE -DCOLORIZE)
endif()

add_executable(artic main.cpp ${BACKEND})
set_target_properties(artic PROPERTIES CXX_STANDARD 17)
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_link_libraries(artic PUBLIC libartic)
