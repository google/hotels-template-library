// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef ATOMIC_SET_H_
#define ATOMIC_SET_H_

#include <atomic>
#include <cassert>
#include <climits>
#include <functional>
#include <optional>

namespace append_only_lookup {
namespace atomic_lookup_internal {
// Round up a value of integral type to the closest power of two
template <typename T>
T inline round_up_base_two(T x) {
  x--;
  for (int i = 1; i < static_cast<int>(sizeof(x) * CHAR_BIT); i *= 2)
    x |= x >> i;
  x++;
  return x;
}

template <typename T, size_t bit_start, size_t bit_len>
struct bit_field {
  using container_type = T;
  static constexpr size_t start = bit_start;
  static constexpr size_t len = bit_len;
};

enum class meta_t : uint64_t { empty = 0 };

// Bit 0 is reserved for "this bucket is occupied"
using meta_occupied_field = bit_field<meta_t, 0, 1>;

// Bit 1 is reserved for "this bucket is constructed"
using meta_constructed_field = bit_field<meta_t, 1, 1>;

// Bits 2 - 63 are reserved for a part of the hash
using meta_partial_hash_field = bit_field<meta_t, 2, 62>;

template <typename T>
T create_mask(size_t start, size_t len) {
  auto mask = (static_cast<T>(1) << len) - 1;
  auto offset = (sizeof(T) * CHAR_BIT - len - start);
  return static_cast<T>(mask) << offset;
}

template <typename Field>
std::underlying_type_t<typename Field::container_type> get_field(
    typename Field::container_type container) {
  constexpr auto start = Field::start;
  constexpr auto len = Field::len;
  constexpr auto total_bits = sizeof(container) * CHAR_BIT;
  auto mask = create_mask<typename Field::container_type>(start, len);
  return (container & mask) >> (total_bits - len - start);
}

template <typename Field>
auto set_field(typename Field::container_type& container,
               std::underlying_type_t<typename Field::container_type> val) {
  using value_type = std::underlying_type_t<typename Field::container_type>;
  constexpr auto start = Field::start;
  constexpr auto len = Field::len;
  constexpr auto total_bits = sizeof(container) * CHAR_BIT;
  auto mask = create_mask<value_type>(start, len);
  // Zero out the bits where the mask is
  auto masked_container = static_cast<value_type>(container) & ~mask;
  masked_container |= (val << total_bits - len - start) & mask;
  container = static_cast<typename Field::container_type>(masked_container);
  // Put the masked value in the correct spot of the masked container
}

meta_t generate_meta(bool occupied, bool constructed, size_t hash) {
  meta_t meta;
  set_field<meta_occupied_field>(meta, occupied);
  set_field<meta_constructed_field>(meta, constructed);
  set_field<meta_partial_hash_field>(meta, hash);
  return meta;
}
}  // namespace atomic_lookup_internal

template <class V, class Hash = std::hash<V>, class Eq = std::equal_to<V>>
class atomic_set {
  struct bucket {
    std::atomic<atomic_lookup_internal::meta_t> meta;
    V val;
  };

  std::vector<bucket> buckets_;
  Hash hasher_;
  Eq eq_;
  size_t mask_;

  static bool bucket_matches(const bucket& b,
                             atomic_lookup_internal::meta_t meta, const V& val,
                             const Eq& eq) {
    return b.meta.load() == meta && eq(b.val, val);
  }

  template <typename Buckets, typename Pred>
  static auto* find_bucket(Buckets& buckets, size_t mask, const Eq& eq,
                           size_t hash, const V& val, Pred&& pred) {
    auto constructed_meta =
        atomic_lookup_internal::generate_meta(true, true, hash);
    for (size_t i = 0; i < buckets.size(); i++) {
      // find the ith bucket in the probe chain
      size_t index = (hash + i) & mask;
      auto& buck = buckets[index];
      if (pred(buck) || bucket_matches(buck, constructed_meta, val, eq))
        return &buck;
    }
    return decltype(&buckets[0])(nullptr);
  }

  template <typename Buckets>
  static auto find_bucket(Buckets& buckets, size_t mask, const Eq& eq,
                          size_t hash, const V& val) {
    // Find the bucket where it belongs
    auto found = find_bucket(buckets, mask, eq, hash, val, [](const auto& b) {
      return b.meta.load() == atomic_lookup_internal::meta_t::empty;
    });

    auto constructed_meta =
        atomic_lookup_internal::generate_meta(true, true, hash);
    // Check if what we found is a valid bucket
    return found && bucket_matches(*found, constructed_meta, val, eq) ? found
                                                                      : nullptr;
  }

 public:
  using value_type = V;
  // Initialize the buckets_ vector with a power of two element count to allow
  // for masking to take an easy modulus
  explicit atomic_set(size_t bucket_count, const Hash& hasher = Hash(),
                      const Eq& eq = Eq())
      : buckets_(atomic_lookup_internal::round_up_base_two(bucket_count)),
        hasher_(hasher),
        eq_(eq),
        mask_(buckets_.size() - 1) {
    assert(bucket_count);
  }

  bool insert(V val) {
    // Find the bucket where it belongs
    const auto hash = hasher_(val);
    auto meta = atomic_lookup_internal::generate_meta(true, false, hash);
    auto found = find_bucket(buckets_, mask_, eq_, hash, val, [&](bucket& b) {
      auto empty = atomic_lookup_internal::meta_t::empty;
      if (b.meta.compare_exchange_strong(empty, meta)) return true;

      // Wait until the bucket's value can be checked for equality
      while (b.meta.load() == meta) std::this_thread::yield();
      return false;
    });
    atomic_lookup_internal::set_field<
        atomic_lookup_internal::meta_constructed_field>(meta, true);

    // If there wasn't a free space or if val is already in the map, fail
    if (!found || found->meta.load() == meta) return false;
    // Store, then atomically "publish", the value and hash in the bucket
    found->val = std::move(val);
    found->meta.store(meta);
    return true;
  }

  bool contains(const V& val) const { return find(val).has_value(); }

  std::optional<V> find(const V& val) const {
    auto found = find_bucket(buckets_, mask_, eq_, hasher_(val), val);
    return found ? std::optional<V>{found->val} : std::nullopt;
  }

  const V* find_ptr(const V& val) const {
    auto found = find_bucket(buckets_, mask_, eq_, hasher_(val), val);
    return found ? &found->val : nullptr;
  }

  V* find_ptr(const V& val) {
    auto found = find_bucket(buckets_, mask_, eq_, hasher_(val), val);
    return found ? &found->val : nullptr;
  }
};

}  // namespace append_only_lookup
#endif  // ATOMIC_SET_H_
