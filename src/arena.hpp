#pragma once
#include <cassert>
#include <cwchar>
#include <functional>
#include <memory>
#include <type_traits>
#include <vector>

constexpr size_t DEFAULT_ARENA_BLOCK_SIZE = 1 << 20; // 1 MB

struct Arena {
    explicit Arena(size_t block_size = DEFAULT_ARENA_BLOCK_SIZE) : default_block_size_(block_size) {
        assert(block_size >= 64);
        allocate_block(default_block_size_);
    }

    ~Arena() {
        reset();
    }

    Arena(const Arena&) = delete;
    Arena& operator=(const Arena&) = delete;

    Arena(Arena&&) = default;
    Arena& operator=(Arena&&) = default;

    template <typename T, typename... Args>
    T* make(Args&&... args) {
        constexpr size_t align = alignof(T);
        const size_t size = sizeof(T);

        void* mem = alloc_bytes(size, align);
        T* obj = ::new(mem) T(std::forward<Args>(args)...);

        if constexpr (!std::is_trivially_destructible_v<T>) {
            destructors_.emplace_back([obj]() {
                obj->~T();
            });
        }
        return obj;
    }

    void* alloc(size_t n, size_t alignment = alignof(max_align_t)) {
        return alloc_bytes(n, alignment);
    }

    template <typename T>
    T* alloc(size_t n = 1) {
        return static_cast<T*>(alloc(n * sizeof(T), alignof(T)));
    }

    void reset() noexcept {
        for (auto it = destructors_.rbegin(); it != destructors_.rend(); ++it) {
            try {
                (*it)();
            } catch (...) {
            }
        }
        destructors_.clear();

        blocks_.clear();
        total_used_ = 0;
        allocate_block(default_block_size_);
    }

    void release_memory() noexcept {
        destructors_.clear();
        blocks_.clear();
        total_used_ = 0;
    }

    [[nodiscard]] size_t used_bytes() const noexcept {
        return total_used_;
    }

    [[nodiscard]] size_t total_bytes() const noexcept {
        size_t s = 0;
        for (auto const& b : blocks_) s += b.size;
        return s;
    }

private:
    struct Block {
        std::unique_ptr<uint8_t[]> data;
        size_t size = 0;
        size_t used = 0;
    };

    std::vector<Block> blocks_;
    std::vector<std::function<void()>> destructors_;
    size_t default_block_size_;
    size_t total_used_{0};

    void allocate_block(size_t size) {
        Block b;
        b.size = std::max<size_t>(size, 64);
        b.used = 0;
        b.data.reset(new uint8_t[b.size]);
        blocks_.push_back(std::move(b));
    }

    void* alloc_bytes(size_t n, size_t align) {
        if (n == 0) return nullptr;
        assert((align & (align - 1)) == 0);

        Block& cur = blocks_.back();
        size_t cur_ptr = reinterpret_cast<size_t>(cur.data.get()) + cur.used;
        size_t aligned_ptr = (cur_ptr + (align - 1)) & ~(align - 1);
        size_t offset = aligned_ptr - reinterpret_cast<size_t>(cur.data.get());

        if (offset + n <= cur.size) {
            void* result = cur.data.get() + offset;
            cur.used = offset + n;
            total_used_ += (offset + n) - cur.used + n;
            return result;
        }

        size_t blk_size = std::max(default_block_size_, n + align);
        allocate_block(blk_size);

        Block& nb = blocks_.back();
        auto new_ptr = reinterpret_cast<size_t>(nb.data.get());
        size_t new_aligned = (new_ptr + (align - 1)) & ~(align - 1);
        size_t new_offset = new_aligned - new_ptr;
        assert(new_offset + n <= nb.size);
        void* result = nb.data.get() + new_offset;
        nb.used = new_offset + n;
        total_used_ += n;
        return result;
    }
};
