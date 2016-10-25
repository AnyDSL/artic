#ifndef ARTIC_H
#define ARTIC_H

#include "irbuilder.h"

namespace artic {

/// Parses the contents of a file.
bool parse(const std::string& file, IRBuilder& builder, std::vector<Expr*>&);
/// Infers the type of an expression.
void infer(const Expr* e);
/// Checks that an expression is correctly typed.
bool check(const Expr* e);
/// Print an expression.
void print(const Expr* e,
           std::ostream& out = std::cout,
           const std::string& tab = "  ",
           int indent = 0, bool color = true);

} // namespace artic

#endif // ARTIC_H
