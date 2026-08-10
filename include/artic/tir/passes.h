#ifndef ARTIC_TIR_PASSES_H
#define ARTIC_TIR_PASSES_H

#include "artic/tir/tir.h"

namespace artic::tir {

bool lower_mod_app(const Module*& module);

}

#endif
