# silly hack because CMake has no plain expressions in its grammar :(
# https://stackoverflow.com/questions/62487808/can-i-set-a-cmake-variable-to-the-result-of-a-boolean-expression
macro(assign_me_bool var)
    if(${ARGN})
        set(${var} ON)
    else()
        set(${var} OFF)
    endif()
endmacro()

function(add_thorin_test)
    cmake_parse_arguments(test "NO_C;NO_LLVM;NO_SPIRV" "NAME;SOURCE_FILE" "ARGS" ${ARGN})
    assign_me_bool(TEST_USE_C NOT ${test_NO_C})
    assign_me_bool(TEST_USE_LLVM ${Thorin_HAS_LLVM_SUPPORT} AND NOT ${test_NO_LLVM})
    assign_me_bool(TEST_USE_SPIRV ${Thorin_HAS_SPIRV_SUPPORT} AND NOT ${test_NO_SPIRV})

    add_test(NAME thorin_${test_NAME} COMMAND ${CMAKE_COMMAND}
            -DCOMPILER=$<TARGET_FILE:artic>
            -DC_COMPILER=${CMAKE_C_COMPILER}
            -DT=${test_NAME}
            "-DTARGS=${test_ARGS}"
            -DSRC=${CMAKE_CURRENT_SOURCE_DIR}/${test_SOURCE_FILE}.art
            -DDST=${CMAKE_CURRENT_BINARY_DIR}
            -DC=${TEST_USE_C}
            -DLLVM=${TEST_USE_LLVM}
            -DSPIRV=${TEST_USE_SPIRV}
    -P ${PROJECT_SOURCE_DIR}/test/thorin/oracle.cmake)
endfunction()

add_thorin_test(NAME hello_world SOURCE_FILE hello_world)
