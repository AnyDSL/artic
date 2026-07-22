#include "artic/tir/tir.h"

#include "artic/print.h"
#include "artic/log.h"

namespace artic {

namespace tir {

void Node::dump() const {
    artic::Printer p(log::out);
    print(p);
    p << '\n';
}

}

}
