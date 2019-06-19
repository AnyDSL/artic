# Parsing
add_test(NAME parse_valid_literals COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/literals.art)
add_test(NAME parse_valid_comments COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/comments.art)
add_test(NAME parse_valid_types    COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/types.art)
add_test(NAME parse_valid_fn       COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/fn.art)
add_test(NAME parse_valid_match    COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/match.art)
add_test(NAME parse_valid_if       COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/if.art)
add_test(NAME parse_valid_while    COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/while.art)
add_test(NAME parse_valid_for      COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/for.art)
add_test(NAME parse_valid_struct   COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/struct.art)
add_test(NAME parse_valid_trait    COMMAND artic --test parse ${PROJECT_SOURCE_DIR}/test/parse/valid/trait.art)

add_test(NAME parse_invalid_comment COMMAND artic --invert --test parse ${PROJECT_SOURCE_DIR}/test/parse/invalid/comment.art)
add_test(NAME parse_invalid_string  COMMAND artic --invert --test parse ${PROJECT_SOURCE_DIR}/test/parse/invalid/string.art)

# Type checking
add_test(NAME check_valid_types    COMMAND artic --test check ${PROJECT_SOURCE_DIR}/test/check/valid/types.art)
add_test(NAME check_valid_loops    COMMAND artic --test check ${PROJECT_SOURCE_DIR}/test/check/valid/loops.art)
