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

#ifndef ATOMIC_MAP_H_
#define ATOMIC_MAP_H_

#include <utility>

#include "append-only-lookup/atomic_set.h"

namespace append-only-lookup{

template <class K, class V, class Hash = std::hash<K>,
          class Eq = std::equal_to<K>>
class atomic_map {
 public:
  using value_type = std::pair<K, V>;

 private:
  static auto bind_hasher(const Hash& hasher) {
    return [](const value_type& vt) { return hasher(vt.first); };
  }

  static auto bind_eq(const Eq& eq) {
    return [](const value_type& lhs, const value_type& rhs) {
      return eq(lhs.first, rhs.first);
    };
  }

  atomic_set<value_type, decltype(bind_hasher(std::declval<Hash>())),
             decltype(bind_eq(std::declval<Eq>()))>
      set_;

 public:
  explicit atomic_map(size_t bucket_count, const Hash& hasher = Hash(),
                      const Eq& eq = Eq())
      : set_(bucket_count, bind_hasher(hasher), bind_eq(eq)) {}

  bool contains(const K& key) const { return set_.contains({key, V{}}); }

  bool insert(K&& key, V&& val) {
    return set_.insert({std::move(key), std::move(val)});
  }

  bool insert(const K& key, const V& val) { return insert(K{key}, V{val}); }

  std::optional<V> at(const K& key) const {
    auto found = set_.find({key, V{}});
    return found ? std::optional<V>{found->second} : std::nullopt;
  }

  V* at_ptr(const K& key) {
    auto found = set_.find_ptr({key, V{}});
    return found ? &found->second : nullptr;
  }

  const V* at_ptr(const K& key) const {
    auto found = set_.find_ptr({key, V{}});
    return found ? &found->second : nullptr;
  }
};

}  // namespace atomic_lookup
#endif  // ATOMIC_MAP_H_
