add_executable(artic main.cpp infer.cpp check.cpp eval.cpp print.cpp types.cpp cast.h infer.h check.h ir.h irbuilder.h loc.h print.h types.h)
set_property(TARGET artic PROPERTY CXX_STANDARD 11)
