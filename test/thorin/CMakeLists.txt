function(add_thorin_test)
    cmake_parse_arguments(test "" "NAME;SOURCE_FILE" "ARGS" ${ARGN})
    add_test(NAME thorin_${test_NAME} COMMAND ${CMAKE_COMMAND}
            -DCOMPILER=$<TARGET_FILE:artic>
            -DC_COMPILER=${CMAKE_C_COMPILER}
            -DT=${test_NAME}
            "-DTARGS=${test_ARGS}"
            -DSRC=${CMAKE_CURRENT_SOURCE_DIR}/${test_SOURCE_FILE}.art
            -DDST=${CMAKE_CURRENT_BINARY_DIR}
            -DLLVM=${Thorin_HAS_LLVM_SUPPORT}
            -DSPIRV=${Thorin_HAS_SPIRV_SUPPORT}
    -P ${PROJECT_SOURCE_DIR}/test/thorin/oracle.cmake)
endfunction()

add_thorin_test(NAME hello_world SOURCE_FILE hello_world)
