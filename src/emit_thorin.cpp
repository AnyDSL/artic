#include "ast.h"

#include <thorin/world.h>
#include <thorin/irbuilder.h>

#include <string>

using namespace thorin;

namespace artic {

static void emit(const ast::FnDecl& fn_decl) {

}

void emit(const std::string& module_name, const ast::Program& program) {
    World world(module_name);
    IRBuilder builder(world);
    for (auto& decl : program.decls) {
        if (decl->isa<ast::FnDecl>())
            emit(*decl->as<ast::FnDecl>());
    }
    world.dump();
}

} // namespace artic
