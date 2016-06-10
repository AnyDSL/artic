#include "lang.h"
#include "check.h"

using namespace artic;

int main(int argc, char** argv) {
    IRBuilder builder;
    PrettyPrinter printer;
    //InferSema infer_sema;
    CheckSema check_sema;

    /*auto fact = let(factorial,
        lambda(x,
            let(c, x == vec(0),
                if_(c,
                    vec(1),
                    let(x_, x - vec(1),
                        let(f_, app(factorial, x_), x * f_))))),
        app(factorial, vec(5)));*/

    auto fact = let(c, vec(1), c);
    auto e = fact();
    //e->infer(infer_sema);
    e->check(check_sema);
    e->print(printer);
    std::cout << std::endl;

    return 0;
}
