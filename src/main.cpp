#include "artic.h"

using namespace artic;

int main(int argc, char** argv) {
    if (argc != 2) return 1;

    IRBuilder builder;
    ExprVec exprs;

    if (parse(argv[1], builder, exprs)) {
        for (auto e : exprs) {
            infer(e);
            //if (check(e)) {
                print(e);
                std::cout << std::endl;
            //}
        }
    }

    return 0;
}
