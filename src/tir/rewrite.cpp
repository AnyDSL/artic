#include "artic/tir/rewrite.h"

#include "artic/tir/types.h"
#include "artic/tir/values.h"

namespace artic {

namespace tir {

const ModType* ModType::rewrite(Rewriter&) const {

}

const PrimType* PrimType::rewrite(Rewriter& r) const {
    return r.dst.prim_type(tag);
}

const NoRetType* NoRetType::rewrite(Rewriter& r) const {
    return r.dst.no_ret_type();
}

const TopType* TopType::rewrite(Rewriter& r) const {
    return r.dst.top_type();
}

const BottomType* BottomType::rewrite(Rewriter& r) const {
    return r.dst.bottom_type();
}

const FnType* FnType::rewrite(Rewriter& r) const {
    const Type* ndom = r.instantiate(dom, true)->as<Type>();
    const Type* ncodom = r.instantiate(codom, true)->as<Type>();
    return r.dst.fn_type(ndom, ncodom);
}

const TupleType* TupleType::rewrite(Rewriter& r) const {
    Array<const Type*> elems;
    for (size_t i = 0; i < elems.size(); ++i) {
        elems[i] = r.instantiate(elems[i], true)->as<Type>();
    }
    return r.dst.tuple_type(elems);
}

const StructType* StructType::rewrite(Rewriter&) const {

}

const EnumType* EnumType::rewrite(Rewriter&) const {

}

const SizedArrayType* SizedArrayType::rewrite(Rewriter& r) const {
    return r.dst.sized_array_type(r.instantiate(elem, true)->as<Type>(), size, is_simd);
}

const UnsizedArrayType* UnsizedArrayType::rewrite(Rewriter& r) const {
    return r.dst.unsized_array_type(r.instantiate(elem, true)->as<Type>());
}

const ImplicitParamType* ImplicitParamType::rewrite(Rewriter& r) const {
    return r.dst.implicit_param_type(r.instantiate(underlying, true)->as<Type>());
}

const PtrType* PtrType::rewrite(Rewriter& r) const {
    return r.dst.ptr_type(r.instantiate(pointee, true)->as<Type>(), is_mut, addr_space);
}

const RefType* RefType::rewrite(Rewriter& r) const {
    return r.dst.ref_type(r.instantiate(pointee, true)->as<Type>(), is_mut, addr_space);
}

const TypeAlias* TypeAlias::rewrite(Rewriter& r) const {

}

const ForallType* ForallType::rewrite(Rewriter&) const {

}

const TypeVar* TypeVar::rewrite(Rewriter&) const {

}

const TypeApp* TypeApp::rewrite(Rewriter&) const {

}

const TypeError* TypeError::rewrite(Rewriter& r) const {
    return r.dst.type_error();
}

Node* Module::rewrite(Rewriter&) const {

}

const Node* GlobalVariable::rewrite(Rewriter&) const {

}

const Node* Fn::rewrite(Rewriter&) const {

}

const Node* Param::rewrite(Rewriter&) const {
    assert(false);
}

Node* App::rewrite(Rewriter&) const {

}

const Node* ImplicitCast::rewrite(Rewriter&) const {

}

const Node* Cast::rewrite(Rewriter&) const {

}

const Node* TypedLiteral::rewrite(Rewriter&) const {

}

const Node* Tuple::rewrite(Rewriter&) const {

}

const Node* Extract::rewrite(Rewriter&) const {

}

const Node* Bind::rewrite(Rewriter&) const {

}

const Node* Seq::rewrite(Rewriter&) const {

}

const Node* Tie::rewrite(Rewriter& r) const {
    return r.instantiate(value, true);
}

const Node* UnOp::rewrite(Rewriter&) const {

}

const Node* BinOp::rewrite(Rewriter&) const {

}

const Node* Branch::rewrite(Rewriter&) const {

}

const Node* Control::rewrite(Rewriter&) const {

}

}

}
