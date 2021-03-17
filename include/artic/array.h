#ifndef ARTIC_ARRAY_H
#define ARTIC_ARRAY_H

#include <cstddef>
#include <memory>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>

namespace artic {

template <typename T> class ArrayRef;

/// Mixin to add immutable storage attributes to an array.
template <typename T, typename Derived>
struct ImmutableStorage {
    const T* data() const { return static_cast<const Derived*>(this)->data(); }
    size_t size() const { return static_cast<const Derived*>(this)->size(); }

    bool empty() const { return size() == 0; }

    const T& front() const { return *begin(); }
    const T& back()  const { return *rbegin(); }

    const T* begin()  const { return data(); }
    const T* end()    const { return data() + size(); }
    const T* rbegin() const { return data() + size() - 1; }
    const T* rend()   const { return data() - 1; }

    const T& operator [] (size_t i) const { return data()[i]; }

    bool operator == (const ImmutableStorage& other) const {
        return other.size() == size() && std::equal(begin(), end(), other.begin());
    }
};

/// Mixin to add mutable storage attributes to an array.
template <typename T, typename Derived>
struct MutableStorage : ImmutableStorage<T, Derived> {
    using ImmutableStorage<T, Derived>::size;
    using ImmutableStorage<T, Derived>::data;
    using ImmutableStorage<T, Derived>::front;
    using ImmutableStorage<T, Derived>::back;
    using ImmutableStorage<T, Derived>::begin;
    using ImmutableStorage<T, Derived>::end;
    using ImmutableStorage<T, Derived>::rbegin;
    using ImmutableStorage<T, Derived>::rend;
    using ImmutableStorage<T, Derived>::operator [];

    T* data() { return static_cast<Derived*>(this)->data(); }

    T& front() { return *begin(); }
    T& back()  { return *rbegin(); }

    T* begin()  { return data(); }
    T* end()    { return data() + size(); }
    T* rbegin() { return data() + size() - 1; }
    T* rend()   { return data() - 1; }

    T& operator [] (size_t i) { return data()[i]; }
};

/// Constant reference to an array.
template <typename T>
class ArrayRef : public ImmutableStorage<T, ArrayRef<T>> {
public:
    ArrayRef(const T* data = nullptr, size_t size = 0)
        : data_(data), size_(size)
    {}

    template <typename Container>
    ArrayRef(const Container& c)
        : ArrayRef(std::data(c), std::size(c))
    {}

    const T* data() const { return data_; }
    size_t size() const { return size_; }

    ArrayRef skip_front(size_t front = 1) const {
        assert(size() >= front);
        return ArrayRef(data() + front, size() - front);
    }

    ArrayRef skip_back(size_t back = 1) const {
        assert(size() >= back);
        return ArrayRef(data(), size() - back);
    }

private:
    const T* data_;
    size_t size_;
};

template<typename Container> ArrayRef(const Container&) -> ArrayRef<typename Container::value_type>;

/// Dynamically-sized, non-growing array.
template <typename T>
class Array : public MutableStorage<T, Array<T>> {
public:
    Array()
        : size_(0)
    {}

    Array(size_t size)
        : data_(std::make_unique<T[]>(size)), size_(size)
    {}

    Array(size_t size, T value)
        : Array(size)
    {
        std::fill(this->begin(), this->end(), value);
    }

    template <typename It>
    Array(It begin, It end)
        : size_(std::distance(begin, end))
    {
        data_.reset(new T[size()]);
        std::copy(begin, end, data_.get());
    }

    Array(std::initializer_list<T> l)
        : Array(l.begin(), l.end())
    {}

    template <typename Container>
    Array(const Container& c)
        : Array(std::begin(c), std::end(c))
    {}

    T* data() { return data_.get(); }
    const T* data() const { return data_.get(); }
    size_t size() const { return size_; }

    void shrink(size_t i) {
        assert(i <= size());
        size_ = i;
    }

private:
    std::unique_ptr<T[]> data_;
    size_t size_;
};

/// Dynamically sized, non-growing array using stack-allocated storage whenever possible.
template <typename T, size_t N = 8>
class SmallArray : public MutableStorage<T, SmallArray<T, N>> {
public:
    SmallArray()
        : size_(0)
    {}

    SmallArray(size_t size)
        : size_(size)
    {
        if (size > N)
            data_ = std::make_unique<T[]>(size);
    }

    SmallArray(size_t size, T value)
        : SmallArray(size)
    {
        std::fill(this->begin(), this->end(), value);
    }

    template <typename It>
    SmallArray(It begin, It end)
        : size_(std::distance(begin, end))
    {
        if (size_ > N)
            data_.reset(new T[size_]);
        std::copy(begin, end, data());
    }

    SmallArray(std::initializer_list<T> l)
        : SmallArray(l.begin(), l.end())
    {}

    template <typename Container>
    SmallArray(const Container& c)
        : SmallArray(std::begin(c), std::end(c))
    {}

    T* data() { return size_ <= N ? buf_ : data_.get(); }
    const T* data() const { return size_ <= N ? buf_ : data_.get(); }
    size_t size() const { return size_; }

    void shrink(size_t i) {
        assert(i <= size());
        if (size_ > N && i <= N)
            std::copy(data_.get(), data_.get() + i, buf_);
        size_ = i;
    }

private:
    T buf_[N];
    std::unique_ptr<T[]> data_;
    size_t size_;
};

} // namespace artic

#endif // ARTIC_ARRAY_H
