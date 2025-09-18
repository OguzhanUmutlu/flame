#pragma once
#include <stdexcept>
#include <new>
#include <utility>
#include <type_traits>

namespace Flame {
    // Minimal std::vector-like implementation, with smaller footprint.
    template <typename T>
    class OldVec_ {
        size_t size_ = 0;
        size_t capacity_ = 0;
        T* data_ = nullptr;

        void prepare_push() {
            if (size_ < capacity_) return;

            const size_t new_cap = capacity_ ? capacity_ + (capacity_ >> 1) : 1;
            T* new_data = static_cast<T*>(::operator new[](new_cap * sizeof(T)));

            size_t i = 0;
            try {
                for (; i < size_; ++i) {
                    ::new (static_cast<void*>(new_data + i)) T(std::move_if_noexcept(data_[i]));
                }
            } catch (...) {
                while (i) {
                    --i;
                    std::destroy_at(new_data + i);
                }
                ::operator delete[](new_data);
                throw;
            }

            // destroy old elements
            if constexpr (!std::is_trivially_destructible_v<T>) {
                for (size_t j = 0; j < size_; ++j) {
                    std::destroy_at(data_ + j);
                }
            }

            ::operator delete[](data_);
            data_ = new_data;
            capacity_ = new_cap;
        }

    public:
        OldVec_() = default;

        ~OldVec_() {
            clear();
            ::operator delete[](data_);
        }

        void push_back(const T& x) {
            prepare_push();
            ::new (static_cast<void*>(data_ + size_)) T(x);
            ++size_;
        }

        void push_back(T&& x) {
            prepare_push();
            ::new (static_cast<void*>(data_ + size_)) T(std::move(x));
            ++size_;
        }

        template <typename... Args>
        void emplace_back(Args&&... args) {
            prepare_push();
            ::new (static_cast<void*>(data_ + size_)) T(std::forward<Args>(args)...);
            ++size_;
        }

        T pop_back() {
            if (size_ == 0) throw std::runtime_error("Cannot pop when no items.");
            T val = std::move(data_[--size_]);
            std::destroy_at(data_ + size_);
            return val;
        }

        T& back() {
            if (size_ == 0) throw std::runtime_error("No elements in Vec.");
            return data_[size_ - 1];
        }

        const T& back() const {
            if (size_ == 0) throw std::runtime_error("No elements in Vec.");
            return data_[size_ - 1];
        }

        void shrink_to_fit() {
            if (size_ < capacity_) {
                const size_t new_cap = size_;
                T* new_data = new_cap ? static_cast<T*>(::operator new[](new_cap * sizeof(T))) : nullptr;

                size_t i = 0;
                try {
                    for (; i < size_; ++i) {
                        ::new (static_cast<void*>(new_data + i)) T(std::move_if_noexcept(data_[i]));
                    }
                } catch (...) {
                    while (i) {
                        --i;
                        std::destroy_at(new_data + i);
                    }
                    ::operator delete[](new_data);
                    throw;
                }

                if constexpr (!std::is_trivially_destructible_v<T>) {
                    for (size_t j = 0; j < size_; ++j) {
                        std::destroy_at(data_ + j);
                    }
                }

                ::operator delete[](data_);
                data_ = new_data;
                capacity_ = new_cap;
            }
        }

        void clear() {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                for (size_t i = 0; i < size_; ++i) {
                    std::destroy_at(data_ + i);
                }
            }
            size_ = 0;
        }

        T& operator[](size_t index) {
            if (index >= size_) throw std::runtime_error("Index out of bounds.");
            return data_[index];
        }

        const T& operator[](size_t index) const {
            if (index >= size_) throw std::runtime_error("Index out of bounds.");
            return data_[index];
        }

        [[nodiscard]] constexpr bool empty() const { return size_ == 0; }
        [[nodiscard]] constexpr size_t size() const { return size_; }
        [[nodiscard]] constexpr size_t capacity() const { return capacity_; }
        [[nodiscard]] T* data() { return data_; }
        [[nodiscard]] const T* data() const { return data_; }

        T* begin() { return data_; }
        T* end() { return data_ + size_; }
        const T* begin() const { return data_; }
        const T* end() const { return data_ + size_; }
        const T* cbegin() const { return data_; }
        const T* cend() const { return data_ + size_; }
    };
}
