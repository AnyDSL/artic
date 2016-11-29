#include <fstream>
#include "artic.h"

using namespace artic;

static bool process_file(const char* file, IRBuilder& builder) {
    std::ifstream is(file);
    if (!is) error("Cannot open file ", file);

    ExprVec exprs;
    if (parse(file, is, builder, exprs)) {
        for (auto e : exprs) {
            infer(e);
            check(e);
            print(e);
            std::cout << std::endl;
        }
    }

    return true;
}

int main(int argc, char** argv) {
    if (argc <= 1) {
        std::cout << "Usage: artic files..." << std::endl;
        return 1;
    }

    IRBuilder builder;
    for (int i = 1; i < argc; i++) {
        if (!process_file(argv[i], builder))
            return 1;
    }

    return 0;
}
