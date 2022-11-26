function(add_failure_test)
    cmake_parse_arguments(test "" "NAME" "COMMAND" ${ARGN})
    add_test(NAME ${test_NAME} COMMAND ${test_COMMAND})
    set_tests_properties(${test_NAME} PROPERTIES WILL_FAIL TRUE)
endfunction()

add_test(NAME version COMMAND artic --version)
add_test(NAME help    COMMAND artic --help)

add_failure_test(NAME invalid_max_errors COMMAND artic --max-errors 0)
add_failure_test(NAME unknown_opt        COMMAND artic --unknown-opt)
add_failure_test(NAME empty_files        COMMAND artic --print-ast)
add_failure_test(NAME cannot_open        COMMAND artic file-that-hopefully-does-not-exist.insane-extension)

add_test(NAME simple_literals1   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/literals1.art)
add_test(NAME simple_literals2   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/literals2.art)
add_test(NAME simple_literal_if  COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/literal_if.art)
add_test(NAME simple_string      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/string.art)
add_test(NAME simple_comments    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/comments.art)
add_test(NAME simple_fn          COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/fn.art)
add_test(NAME simple_match1      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/match1.art)
add_test(NAME simple_match2      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/match2.art)
add_test(NAME simple_match3      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/match3.art)
add_test(NAME simple_match4      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/match4.art)
add_test(NAME simple_if          COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/if.art)
add_test(NAME simple_if_let      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/if_let.art)
add_test(NAME simple_implicit1   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/implicit1.art)
add_test(NAME simple_implicit2   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/implicit2.art)
add_test(NAME simple_while       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/while.art)
add_test(NAME simple_while_let   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/while_let.art)
add_test(NAME simple_for         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/for.art)
add_test(NAME simple_structs1    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs1.art)
add_test(NAME simple_structs2    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs2.art)
add_test(NAME simple_structs3    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs3.art)
add_test(NAME simple_structs4    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs4.art)
add_test(NAME simple_structs5    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs5.art)
add_test(NAME simple_structs6    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/structs6.art)
add_test(NAME simple_tuple_like1 COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/tuple_like1.art)
add_test(NAME simple_tuple_like2 COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/tuple_like2.art)
add_test(NAME simple_proj        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/proj.art)
add_test(NAME simple_enums1      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums1.art)
add_test(NAME simple_enums2      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums2.art)
add_test(NAME simple_enums3      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums3.art)
add_test(NAME simple_enums4      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/enums4.art)
add_test(NAME simple_types       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/types.art)
add_test(NAME simple_loops       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/loops.art)
add_test(NAME simple_return      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/return.art)
add_test(NAME simple_mut         COMMAND artic --print-ast --warnings-as-errors ${CMAKE_CURRENT_SOURCE_DIR}/simple/mut.art)
add_test(NAME simple_static      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/static.art)
add_test(NAME simple_arrays1     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/arrays1.art)
add_test(NAME simple_arrays2     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/arrays2.art)
add_test(NAME simple_sort        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/sort.art)
add_test(NAME simple_filters1    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/filters1.art)
add_test(NAME simple_filters2    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/filters2.art)
add_test(NAME simple_ops         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/ops.art)
add_test(NAME simple_mod1        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/mod1.art)
add_test(NAME simple_mod2        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/mod2.art)
add_test(NAME simple_mod3        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/mod3.art)
add_test(NAME simple_use         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/use.art)
add_test(NAME simple_poly_fn1    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/poly_fn1.art)
add_test(NAME simple_poly_fn2    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/poly_fn2.art)
add_test(NAME simple_nested_fns  COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/nested_fns.art)
add_test(NAME simple_address     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/address.art)
add_test(NAME simple_church      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/church.art)
add_test(NAME simple_sort_nets   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/sort_nets.art)
add_test(NAME simple_regex       COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/regex.art)
add_test(NAME simple_cc          COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/cc.art)
add_test(NAME simple_escape      COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/escape.art)
add_test(NAME simple_asm         COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/asm.art)
add_test(NAME simple_simd        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/simd.art)
add_test(NAME simple_type_args   COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/type_args.art)
add_test(NAME simple_subtype1    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/subtype1.art)
add_test(NAME simple_subtype2    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/subtype2.art)
add_test(NAME simple_subtype3    COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/subtype3.art)
add_test(NAME simple_compare     COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/compare.art)
add_test(NAME simple_double_ptr  COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/double_ptr.art)
add_test(NAME simple_math        COMMAND artic --print-ast ${CMAKE_CURRENT_SOURCE_DIR}/simple/math.art)

add_failure_test(NAME failure_annot          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/annot.art)
add_failure_test(NAME failure_comment        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/comment.art)
add_failure_test(NAME failure_utf8           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/utf8.art)
add_failure_test(NAME failure_dots           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/dots.art)
add_failure_test(NAME failure_char           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/char.art)
add_failure_test(NAME failure_literals       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/literals.art)
add_failure_test(NAME failure_similar        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/similar.art)
add_failure_test(NAME failure_string         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/string.art)
add_failure_test(NAME failure_params         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/params.art)
add_failure_test(NAME failure_bind1          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/bind1.art)
add_failure_test(NAME failure_bind2          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/bind2.art)
add_failure_test(NAME failure_unreachable1   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/unreachable1.art)
add_failure_test(NAME failure_unreachable2   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/unreachable2.art)
add_failure_test(NAME failure_unreachable3   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/unreachable3.art)
add_failure_test(NAME failure_uninferable1   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/uninferable1.art)
add_failure_test(NAME failure_uninferable2   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/uninferable2.art)
add_failure_test(NAME failure_uninferable3   COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/uninferable3.art)
add_failure_test(NAME failure_noret          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/noret.art)
add_failure_test(NAME failure_arrays1        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/arrays1.art)
add_failure_test(NAME failure_arrays2        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/arrays2.art)
add_failure_test(NAME failure_if             COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/if.art)
add_failure_test(NAME failure_if_let         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/if_let.art)
add_failure_test(NAME failure_implicit1      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/implicit1.art)
add_failure_test(NAME failure_implicit2      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/implicit2.art)
add_failure_test(NAME failure_while_let      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/while_let.art)
add_failure_test(NAME failure_structs1       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs1.art)
add_failure_test(NAME failure_structs2       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs2.art)
add_failure_test(NAME failure_structs3       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs3.art)
add_failure_test(NAME failure_structs4       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs4.art)
add_failure_test(NAME failure_structs5       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs5.art)
add_failure_test(NAME failure_structs6       COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/structs6.art)
add_failure_test(NAME failure_tuple_like1    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/tuple_like1.art)
add_failure_test(NAME failure_tuple_like2    COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/tuple_like2.art)
add_failure_test(NAME failure_proj           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/proj.art)
add_failure_test(NAME failure_enums1         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums1.art)
add_failure_test(NAME failure_enums2         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums2.art)
add_failure_test(NAME failure_enums3         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums3.art)
add_failure_test(NAME failure_enums4         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/enums4.art)
add_failure_test(NAME failure_mut            COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mut.art)
add_failure_test(NAME failure_filter1        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter1.art)
add_failure_test(NAME failure_filter2        COMMAND artic --warnings-as-errors ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter2.art)
add_failure_test(NAME failure_filter3        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter3.art)
add_failure_test(NAME failure_filter4        COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/filter4.art)
add_failure_test(NAME failure_match1         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/match1.art)
add_failure_test(NAME failure_match2         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/match2.art)
add_failure_test(NAME failure_param          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/param.art)
add_failure_test(NAME failure_ops            COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/ops.art)
add_failure_test(NAME failure_mod1           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mod1.art)
add_failure_test(NAME failure_mod2           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mod2.art)
add_failure_test(NAME failure_mod3           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mod3.art)
add_failure_test(NAME failure_mod4           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/mod4.art)
add_failure_test(NAME failure_use1           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/use1.art)
add_failure_test(NAME failure_use2           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/use2.art)
add_failure_test(NAME failure_use3           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/use3.art)
add_failure_test(NAME failure_use4           COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/use4.art)
add_failure_test(NAME failure_static         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/static.art)
add_failure_test(NAME failure_cc             COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/cc.art)
add_failure_test(NAME failure_escape         COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/escape.art)
add_failure_test(NAME failure_asm            COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/asm.art)
add_failure_test(NAME failure_simd1          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/simd1.art)
add_failure_test(NAME failure_simd2          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/simd2.art)
add_failure_test(NAME failure_type_args1     COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/type_args1.art)
add_failure_test(NAME failure_type_args2     COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/type_args2.art)
add_failure_test(NAME failure_cast1          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/cast1.art)
add_failure_test(NAME failure_cast2          COMMAND artic --warnings-as-errors ${CMAKE_CURRENT_SOURCE_DIR}/failure/cast2.art)
add_failure_test(NAME failure_attrs          COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/attrs.art)
add_failure_test(NAME failure_not_written_to COMMAND artic --warnings-as-errors ${CMAKE_CURRENT_SOURCE_DIR}/failure/not_written_to.art)
add_failure_test(NAME failure_not_sized      COMMAND artic ${CMAKE_CURRENT_SOURCE_DIR}/failure/not_sized.art)

set(CODEGEN_TESTS "")
if (Thorin_HAS_LLVM_SUPPORT)
    # This version is required for the --ignore-eol flag used when comparing files
    cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

    find_package(Clang REQUIRED CONFIG PATHS ${LLVM_DIR}/../clang NO_DEFAULT_PATH)

    # Compile the helper functions into an object file
    set(HELPERS_OBJ ${CMAKE_CURRENT_BINARY_DIR}/test_helpers.o)
    add_custom_command(
        OUTPUT ${HELPERS_OBJ}
        COMMAND
            $<TARGET_FILE:clang>
            -c ${CMAKE_CURRENT_SOURCE_DIR}/codegen/helpers.c
            -o ${HELPERS_OBJ}
        DEPENDS clang ${CMAKE_CURRENT_SOURCE_DIR}/codegen/helpers.c)
    add_custom_target(test_helpers ALL DEPENDS ${HELPERS_OBJ})

    # Look for libm, which is necessary on *nix systems when
    # using transcendental functions like sin() and friends
    include(CheckLibraryExists)
    check_library_exists(m sin "" HAS_MATH_LIB)
    set(MATH_LIB "")
    if (HAS_MATH_LIB)
        set(MATH_LIB "-lm")
    endif ()

    function(add_codegen_test)
        cmake_parse_arguments(test "" "NAME;SOURCE_FILE;REFERENCE" "ARGS" ${ARGN})
        # The test executable has to be linked with clang, because on some distros,
        # gcc refuses to link properly the object file generated by clang.
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test_${test_NAME}
            COMMAND $<TARGET_FILE:artic> ${test_SOURCE_FILE} --emit-llvm -o ${test_NAME}
            COMMAND $<TARGET_FILE:clang> ${test_NAME}.ll ${MATH_LIB} ${HELPERS_OBJ} -o test_${test_NAME}
            DEPENDS artic clang test_helpers ${test_SOURCE_FILE}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        add_custom_target(test_${test_NAME} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/test_${test_NAME})
        add_test(
            NAME ${test_NAME}
            COMMAND
                ${CMAKE_COMMAND}
                "-DTEST_NAME=${test_NAME}"
                "-DTEST_EXECUTABLE=${CMAKE_CURRENT_BINARY_DIR}/test_${test_NAME}"
                "-DTEST_REFERENCE=${test_REFERENCE}"
                "-DTEST_ARGS=${test_ARGS}"
                -P ${CMAKE_CURRENT_SOURCE_DIR}/run_codegen_test.cmake
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        list(APPEND CODEGEN_TESTS test_${test_NAME})
        set(CODEGEN_TESTS ${CODEGEN_TESTS} PARENT_SCOPE)
    endfunction()

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
    add_codegen_test(
        NAME codegen_mandelbrot
        ARGS 1024
        SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/mandelbrot.art
        REFERENCE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/mandelbrot.ref)
    add_codegen_test(
        NAME compare
        SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/codegen/compare.art)
endif ()

if (CODE_COVERAGE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(COVERAGE_EXCLUDES "${CMAKE_CURRENT_SOURCE_DIR}/codegen/*")
    setup_target_for_coverage_gcovr_html(
        NAME coverage
        EXECUTABLE ctest -j ${PROCESSOR_COUNT}
        DEPENDENCIES artic ${CODEGEN_TESTS})
endif ()
