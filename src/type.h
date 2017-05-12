#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <vector>

#include "cast.h"
#include "hash.h"
#include "box.h"
#include "print.h"
#include "token.h"

struct Type : public Cast<Type> {
    virtual ~Type() {}

    bool is_tuple() const;

    virtual uint32_t hash() const = 0;
    virtual bool equals(const Type*) const = 0;
    virtual void print(Printer&) const = 0;
};

struct PrimType : public Type {
    enum Tag {
#define TAG(t, n, ty) t = Box::t,
        PRIM_TAGS(TAG)
#undef TAG
        ERR
    };

    Tag tag;

    PrimType(Tag tag)
        : tag(tag)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;

    static std::string tag_to_string(Tag tag);
    static Tag tag_from_token(const Token&);
};

struct TupleType : public Type {
    std::vector<const Type*> args;

    TupleType(std::vector<const Type*>&& args)
        : args(args)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

struct FunctionType : public Type {
    const Type* from;
    const Type* to;

    FunctionType(const Type* from, const Type* to)
        : from(from), to(to)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

struct ErrorType : public Type {
    const Loc& loc;

    ErrorType(const Loc& loc)
        : loc(loc)
    {}

    uint32_t hash() const override;
    bool equals(const Type* t) const override;
    void print(Printer&) const override;
};

class TypeTable {
public:
    ~TypeTable() {
        for (auto& t : types_) delete t;
    }

    template <typename T, typename... Args>
    const T* new_type(Args... args) {
        T t(std::forward<Args>(args)...);
        auto it = types_.find(&t);
        if (it != types_.end()) {
            assert(t.equals(*it));
            return (*it)->template as<T>();
        }
        const T* ptr = new T(std::move(t));
        types_.emplace(ptr);
        return ptr;
    }

private:
    struct HashType {
        size_t operator () (const Type* t) const {
            return size_t(t->hash());
        }
    };

    struct CmpType {
        bool operator () (const Type* a, const Type* b) const {
            return a->equals(b);
        }
    };

    std::unordered_set<const Type*, HashType, CmpType> types_;
};

#endif // TYPE_H
