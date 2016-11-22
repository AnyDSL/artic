add_executable(artic main.cpp parse.cpp infer.cpp check.cpp eval.cpp print.cpp types.cpp cast.h ir.h ir_builder.h loc.h types.h common.h substitution.h)
set_property(TARGET artic PROPERTY CXX_STANDARD 11)
