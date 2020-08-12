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

#ifndef SYNCHRONIZED_MAP_H
#define SYNCHRONIZED_MAP_H

#include "synchronized-value/synchronized_value.h"
#include "unordered_map"

namespace append-only-lookup {
template <typename K, typename V, typename Strategy>
class synchronized_map {
  synchronized_value::synchronized_value<std::unordered_map<K, V>, Strategy> map_;
public:
  explicit synchronized_map() = default;
  explicit synchronized_map(int size) : map_(size) {}
  
  bool contains(const K& key) {
    return map_.read([&](const auto& map){return map.count(key);});
  }
  
  bool insert(K key, V val) {
    bool result = false;
    map_.update_inplace([&](auto& map){result = map.insert(std::make_pair(std::move(key), std::move(val))).second; });
    return result;
  }
  
  std::optional<V> at(const K& key) {
    return map_.read([&](const auto& map){
      auto found = map.find(key);
      return found != map.end() ? std::optional<V>{found->second} : std::nullopt;
    });
  }
};
}  // namespace append-only-lookup

#endif  // SYNCHRONIZED_MAP_H
