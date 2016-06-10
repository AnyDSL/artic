#include "lang.h"
#include "check.h"

using namespace artic;

int main(int argc, char** argv) {
    Lang lang;
    PrettyPrinter printer;
    //InferSema infer_sema;
    CheckSema check_sema;

#include "begin_dsl.h"
    auto fact = let(factorial) be
        lambda(x)
            let(c) be vec(0) == var(x) in
                if var(c) then
                    vec(1)
                else
                    let(x_) be var(x) - vec(1) in
                        let(f_) be app(factorial, var(x_)) in
                            x * var(f_)
                        end
                    end
                end
            end
        end
    in
        vec(1, 1, 1, 1)
    end;
#include "end_dsl.h"

    auto e = fact();
    //e->infer(infer_sema);
    //e->check(check_sema);
    e->print(printer);
    std::cout << std::endl;

    return 0;
}
