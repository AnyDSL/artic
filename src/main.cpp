#include <fstream>
#include "artic.h"

using namespace artic;

int main(int argc, char** argv) {
    if (argc != 2) return 1;

    IRBuilder builder;
    ExprVec exprs;

    std::ifstream file(argv[1]);
    if (!file) {
        error("Cannot open file ", argv[1]);
        return 1;
    }

    if (parse(argv[1], file, builder, exprs)) {
        for (auto e : exprs) {
            infer(e);check(e);
            //if (check(e)) {
                print(e);
                std::cout << std::endl;
            //}
        }
    }

    return 0;
}
