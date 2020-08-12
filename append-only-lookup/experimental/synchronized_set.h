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

#ifndef SYNCHRONIZED_SET_H
#define SYNCHRONIZED_SET_H

#include "synchronized-value/synchronized_value.h"
#include <unordered_set>

namespace append_only_lookup {
template <typename V, typename Strategy>
class synchronized_set {
  synchronized_value::synchronized_value<std::unordered_set<V>, Strategy> set_;
public:
  using value_type = V;
  synchronized_set() = default;
  synchronized_set(int size) : set_(size) {}
  
  bool contains(const V& val) {
    return set_.read([&](const auto& set){return set.count(val);});
  }
  
  bool insert(V val) {
    bool result = false;
    set_.update_inplace([&](auto& set){result = set.insert(std::move(val)).second; });
    return result;
  }
  
  std::optional<V> find(const V& val) {
    return set_.read([&](const auto& set){
      auto found = set.find(val);
      return found == set.end() ? std::nullopt : std::optional<V>{*found};
    });
  }
};
}  // namespace append_only_lookup

#endif  // SYNCHRONIZED_SET_H
