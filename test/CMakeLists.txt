function(add_failure_test)
    cmake_parse_arguments(test "" "NAME" "COMMAND" ${ARGN})
    add_test(NAME ${test_NAME} COMMAND ${test_COMMAND})
    set_tests_properties(${test_NAME} PROPERTIES WILL_FAIL TRUE)
endfunction()

function(add_codegen_test)
    cmake_parse_arguments(test "" "NAME;SOURCE_FILE;REFERENCE" "ARGS" ${ARGN})
    add_test(
        NAME ${test_NAME}
        COMMAND
            ${CMAKE_COMMAND}
            -DARTIC_COMMAND=$<TARGET_FILE:artic>
            -DCLANG_COMMAND=$<TARGET_FILE:clang>
            -DTEST_NAME=${test_NAME}
            -DTEST_SOURCE_FILE=${test_SOURCE_FILE}
            -DTEST_REFERENCE=${test_REFERENCE}
            "-DTEST_ARGS=${test_ARGS}"
            -DHELPERS_FILE=${CMAKE_CURRENT_SOURCE_DIR}/codegen/helpers.c
            -P ${CMAKE_CURRENT_SOURCE_DIR}/run_codegen_test.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endfunction()

add_test(NAME version COMMAND artic --version)
add_test(NAME help    COMMAND artic --help)

add_test(NAME dup_strict  COMMAND artic --strict --strict)
add_test(NAME unknown_opt COMMAND artic --unknown-opt)
add_test(NAME empty_files COMMAND artic --strict)
add_test(NAME cannot_open COMMAND artic file-that-hopefully-does-not-exist.insane-extension)
add_test(NAME open_dir    COMMAND artic ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(dup_strict unknown_opt empty_files cannot_open open_dir PROPERTIES WILL_FAIL TRUE)

add_test(NAME simple_literals   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/literals.art)
add_test(NAME simple_string     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/string.art)
add_test(NAME simple_comments   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/comments.art)
add_test(NAME simple_fn         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/fn.art)
add_test(NAME simple_match      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/match.art)
add_test(NAME simple_if         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/if.art)
add_test(NAME simple_while      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/while.art)
add_test(NAME simple_for        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/for.art)
add_test(NAME simple_structs1   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs1.art)
add_test(NAME simple_structs2   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs2.art)
add_test(NAME simple_structs3   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs3.art)
add_test(NAME simple_structs4   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs4.art)
add_test(NAME simple_structs5   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs5.art)
add_test(NAME simple_enums1     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums1.art)
add_test(NAME simple_enums2     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums2.art)
add_test(NAME simple_enums3     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums3.art)
add_test(NAME simple_types      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/types.art)
add_test(NAME simple_loops      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/loops.art)
add_test(NAME simple_return     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/return.art)
add_test(NAME simple_mut        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/mut.art)
add_test(NAME simple_static     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/static.art)
add_test(NAME simple_arrays1    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/arrays1.art)
add_test(NAME simple_arrays2    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/arrays2.art)
add_test(NAME simple_sort       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/sort.art)
add_test(NAME simple_filters1   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/filters1.art)
add_test(NAME simple_filters2   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/filters2.art)
add_test(NAME simple_ops        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/ops.art)
add_test(NAME simple_mod        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/mod.art)
add_test(NAME simple_poly_fn    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/poly_fn.art)
add_test(NAME simple_nested_fns COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/nested_fns.art)
add_test(NAME simple_address    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/address.art)
add_test(NAME simple_church     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/church.art)
add_test(NAME simple_sort_nets  COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/sort_nets.art)
add_test(NAME simple_regex      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/regex.art)
add_test(NAME simple_cc         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/cc.art)
add_test(NAME simple_escape     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/escape.art)
add_test(NAME simple_asm        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/asm.art)
add_test(NAME simple_simd       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/simd.art)

add_failure_test(NAME failure_annot       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/annot.art)
add_failure_test(NAME failure_comment     COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/comment.art)
add_failure_test(NAME failure_utf8        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/utf8.art)
add_failure_test(NAME failure_dots        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/dots.art)
add_failure_test(NAME failure_char        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/char.art)
add_failure_test(NAME failure_literals    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/literals.art)
add_failure_test(NAME failure_similar     COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/similar.art)
add_failure_test(NAME failure_string      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/string.art)
add_failure_test(NAME failure_params      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/params.art)
add_failure_test(NAME failure_bind        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/bind.art)
add_failure_test(NAME failure_unreachable COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/unreachable.art)
add_failure_test(NAME failure_uninferable COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/uninferable.art)
add_failure_test(NAME failure_noret       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/noret.art)
add_failure_test(NAME failure_arrays      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/arrays.art)
add_failure_test(NAME failure_structs1    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs1.art)
add_failure_test(NAME failure_structs2    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs2.art)
add_failure_test(NAME failure_structs3    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs3.art)
add_failure_test(NAME failure_structs4    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs4.art)
add_failure_test(NAME failure_enums1      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums1.art)
add_failure_test(NAME failure_enums2      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums2.art)
add_failure_test(NAME failure_mut         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mut.art)
add_failure_test(NAME failure_filter1     COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter1.art)
add_failure_test(NAME failure_filter2     COMMAND artic --strict ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter2.art)
add_failure_test(NAME failure_static      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/static.art)
add_failure_test(NAME failure_cc          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/cc.art)
add_failure_test(NAME failure_escape      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/escape.art)
add_failure_test(NAME failure_asm         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/asm.art)
add_failure_test(NAME failure_simd1       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/simd1.art)
add_failure_test(NAME failure_simd2       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/simd2.art)

if (Thorin_HAS_LLVM_SUPPORT)
    find_package(Clang REQUIRED CONFIG PATHS ${LLVM_DIR}/../clang NO_DEFAULT_PATH)

    add_codegen_test(
        NAME codegen_fannkuch
        ARGS 8
        SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/fannkuch.art
        REFERENCE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/fannkuch.ref)
    add_codegen_test(
        NAME codegen_meteor
        ARGS 2098
        SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/meteor.art
        REFERENCE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/meteor.ref)
    add_codegen_test(
        NAME codegen_aobench
        ARGS ""
        SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/aobench.art
        REFERENCE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/aobench.ref)
endif ()

if(CODE_COVERAGE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    setup_target_for_coverage_gcovr_html(NAME coverage EXECUTABLE ctest -j ${PROCESSOR_COUNT} DEPENDENCIES artic)
endif ()
