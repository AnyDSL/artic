#ifndef ARTIC_ARENA_H
#define ARTIC_ARENA_H

#include <type_traits>
#include <memory>
#include <vector>

template<typename T>
/** works like unique_ptr but doesn't actually own anything */
struct arena_ptr {
    T* _ptr;

    arena_ptr() : _ptr(nullptr) {}
    arena_ptr(T* ptr) : _ptr(ptr) {}

    // template<typename S, std::enable_if_t<std::is_convertible_v<S*, T*>, bool> = true>
    // arena_ptr(arena_ptr<S>& other) : _ptr(other._ptr) {}

    template<typename S, std::enable_if_t<std::is_convertible_v<S*, T*>, bool> = true>
    arena_ptr(arena_ptr<S>&& other) : _ptr(other._ptr) {
        other._ptr = nullptr;
    }
    ~arena_ptr() {
        _ptr = nullptr;
    }

    // arena_ptr<T>& operator=(const arena_ptr<T>& other) { _ptr = other._ptr; return *this; }
    arena_ptr<T>& operator=(arena_ptr<T>&& other) { _ptr = other._ptr; other._ptr = nullptr; return *this; }

    T* operator->() const { return _ptr; }
    T& operator*() const { return *_ptr; }
    operator bool() const { return _ptr; }

    T* get() const { return _ptr; }

    void swap(arena_ptr<T>& other) {
        T* tmp = other._ptr;
        other._ptr = _ptr;
        _ptr = tmp;
    }
};

struct Arena {
    Arena();
    ~Arena();

    template<typename T, typename ...Args>
    arena_ptr<T> make_ptr(Args&& ...args) {
        void* ptr = alloc(sizeof(T));
        new (ptr) T (std::forward<Args>(args)...);
        return arena_ptr<T>(static_cast<T*>(ptr));
    }
private:
    void* alloc(size_t);
    void grow();

    size_t _block_size;
    size_t _available;
    std::vector<void*> _data;
};

#endif // ARTIC_ARENA_H
