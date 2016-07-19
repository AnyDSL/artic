#include "lang.h"
#include "check.h"
#include "infer.h"

using namespace artic;

int main(int argc, char** argv) {
    IRBuilder builder;
    PrettyPrinter printer;
    InferSema infer_sema;
    CheckSema check_sema;

#include "begin_dsl.h"
    auto fact = let(factorial) =
        lambda(x)
            let(c) = vec(0) == x in
                if c then
                    vec(1)
                else
                    let(x_) = x - vec(1) in
                        let(f_) = app(factorial, x_) in
                            x * f_
                        end_let
                    end_let
                end_if
            end_let
        end_lambda
    in
        vec(1, 2, 3, 4)
    end_let;
#include "end_dsl.h"

    auto e = fact(Loc(__FILE__, __LINE__));

    infer_sema.infer(e);
    check_sema.check(e);

    e->print(printer);
    std::cout << std::endl;

    return 0;
}
