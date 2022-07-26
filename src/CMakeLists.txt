add_library(libartic
    ../include/artic/ast.h
    ../include/artic/bind.h
    ../include/artic/cast.h
    ../include/artic/check.h
    ../include/artic/emit.h
    ../include/artic/lexer.h
    ../include/artic/loc.h
    ../include/artic/locator.h
    ../include/artic/log.h
    ../include/artic/parser.h
    ../include/artic/print.h
    ../include/artic/summoner.h
    ../include/artic/symbol.h
    ../include/artic/token.h
    ../include/artic/types.h
    ast.cpp
    bind.cpp
    check.cpp
    emit.cpp
    lexer.cpp
    log.cpp
    parser.cpp
    print.cpp
    summoner.cpp
    types.cpp)

set_target_properties(libartic PROPERTIES PREFIX "" CXX_STANDARD 17)

target_link_libraries(libartic PUBLIC ${Thorin_LIBRARIES})
target_include_directories(libartic PUBLIC ${Thorin_INCLUDE_DIRS} ../include)
target_compile_definitions(libartic PRIVATE -DARTIC_EXPORT)
if (${COLORIZE})
    target_compile_definitions(libartic PUBLIC -DCOLORIZE)
endif()

add_executable(artic main.cpp)
set_target_properties(artic PROPERTIES CXX_STANDARD 17)
target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_link_libraries(artic PUBLIC libartic)
if (Thorin_HAS_JSON_SUPPORT)
    target_compile_definitions(artic PUBLIC -DENABLE_JSON)
    if (NOT TARGET nlohmann_json)
        find_package(nlohmann_json 3.2.0 REQUIRED)
    endif()
    target_link_libraries(artic PUBLIC nlohmann_json::nlohmann_json)
endif()
if (Thorin_HAS_LLVM_SUPPORT)
    target_compile_definitions(artic PUBLIC -DENABLE_LLVM)
    llvm_config(artic ${AnyDSL_LLVM_LINK_SHARED} core support)
endif ()

target_link_libraries(artic PUBLIC dl)
