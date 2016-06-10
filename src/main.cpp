#include "lang.h"

using namespace artic;

int main(int argc, char** argv) {
    IRBuilder b;
    PrettyPrinter p;

    auto fact = let(factorial,
        lambda(x,
            let(c, x == vec(0),
                if_(c,
                    vec(1),
                    let(x_, x - vec(1),
                        let(f_, app(factorial, x_), x * f_))))),
        app(factorial, vec(5)));
    fact()->print(p);
    std::cout << std::endl;
    return 0;
}
