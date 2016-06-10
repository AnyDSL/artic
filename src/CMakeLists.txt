add_executable(artic main.cpp check.cpp eval.cpp print.cpp types.cpp cast.h check.h ir.h irbuilder.h loc.h print.h types.h)
set_property(TARGET artic PROPERTY CXX_STANDARD 11)