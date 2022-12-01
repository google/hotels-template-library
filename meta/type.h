// Copyright 2021 Google LLC
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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_H_

#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

#include "meta/basic_tuple.h"

namespace htls::meta {
namespace internal_type {

template <typename T, typename F, typename = void>
struct IsValidExprImpl : std::false_type {};
template <typename T, typename F>
struct IsValidExprImpl<T, F, std::void_t<std::invoke_result_t<F, T>>>
    : std::true_type {};

template <typename T>
constexpr std::string_view GetRawTypeName() {
  return __PRETTY_FUNCTION__;
}

inline constexpr std::string_view long_double_raw_type_name =
    GetRawTypeName<long double>();
inline constexpr std::string_view long_double_type_name = "long double";
inline constexpr std::size_t type_name_prefix =
    long_double_raw_type_name.find(long_double_type_name);
static_assert(type_name_prefix != std::string_view::npos);
inline constexpr std::size_t type_name_suffix =
    long_double_raw_type_name.size() -
    (type_name_prefix + long_double_type_name.size());

template <typename T>
constexpr std::string_view GetDebugTypeName() {
  std::string_view raw_name = GetRawTypeName<T>();
  raw_name.remove_prefix(type_name_prefix);
  raw_name.remove_suffix(type_name_suffix);
  return raw_name;
}

}  // namespace internal_type
//
// Holds exactly one arbitrary type to enable value semantics for types.
//
// Type is similar to boost::hana::basic_type.
// https://www.boost.org/doc/libs/1_61_0/libs/hana/doc/html/index.html#tutorial-type
template <typename T>
struct Type {
  using type = T;
  constexpr Type() = default;

  // This CTOR doesn't work for T == void so we use SFINAE to only conditionally
  // enable it.
  template <typename U, typename = std::enable_if_t<std::is_same_v<T, U>>>
  constexpr explicit Type(const U&) {}
};
template <typename T>
Type(const T&) -> Type<T>;

template <typename T>
constexpr Type<T> type_c;

// Two Types are equal if the types they hold are equal.
template <typename T, typename U>
constexpr bool operator==(Type<T>, Type<U>) {
  return std::is_same_v<Type<T>, Type<U>>;
}

template <typename T, typename U>
constexpr bool operator!=(Type<T> a, Type<U> b) {
  return !(a == b);
}

// SFINAE-like helper to determine if an expression is valid.
//
//   if constexpr (
//       IsValidExpr(x, [](const auto& t) -> decltype(std::cout << t) {})) {
//     std::cout << x << "\n";
//   } else {
//     std::cout << "x was not printable\n";
//   }
template <typename T, typename F>
constexpr bool IsValidExpr(T&& t, F&& f) {
  return internal_type::IsValidExprImpl<T, F>::value;
}

// SFINAE-like helper to perform an operation if possible or return a default if
// not.
//
//   auto member = ValidExprOr(
//       x,
//       [](const auto& t) -> decltype(t.member) { return t.member; },
//       absl::nullopt);
template <typename T, typename F, typename U>
constexpr decltype(auto) ValidExprOr(T&& t, F&& f, U&& u) {
  if constexpr (internal_type::IsValidExprImpl<T, F>::value) {
    return std::forward<F>(f)(std::forward<T>(t));
  } else {
    return std::forward<U>(u);
  }
}

// Makes a BasicTuple of Types out of the template arguments provided to a
// class template.
template <template <typename...> typename T, typename... Ts>
constexpr auto AsTuple(Type<T<Ts...>>) {
  return htls::meta::BasicTuple<Type<Ts>...>();
}

// Makes a Type of a class template instance using the types from a
// BasicTuple of Types.
template <template <typename...> typename T, typename... Ts>
constexpr auto FromTuple(htls::meta::BasicTuple<Type<Ts>...>) {
  return Type<T<Ts...>>();
}

// Provide a functor like interface to a template meta function which has a
// value or type. Also supports partial application.
//
// MetaValueFunction matches the interface expected by Filter, which allows
// traditional template meta functions to be used with Filter easily.
//
// These two forms are equivalent:
//   std::is_same_v<A, B>
//   MetaValueFunction<std::is_same, A>()(Type<B>())
template <template <typename...> typename F, typename... Ts>
struct MetaValueFunction {
  template <typename... Us>
  constexpr auto operator()(Type<Us>...) const {
    return F<Ts..., Us...>::value;
  }
};

template <template <typename...> typename F, typename... Ts>
struct MetaTypeFunction {
  template <typename... Us>
  constexpr auto operator()(Type<Us>...) const {
    return Type<typename F<Ts..., Us...>::type>();
  }
};

// Get the size of a BasicTuple type.
template <typename... Ts>
constexpr std::size_t size(Type<htls::meta::BasicTuple<Ts...>>) {
  return sizeof...(Ts);
}

// Get the debug string name of a type T.
//   DebugTypName(type_c<int>) == "int"
template <typename T>
constexpr std::string_view DebugTypeName(Type<T>) {
  return internal_type::GetDebugTypeName<T>();
}

////////////////////////////////////////////////////////////////////////////////
// Check if the function argument is a template instance of the template
// argument; e.g.
//   IsTemplateInstance<std::optional>(type_c<std::optional<int>>) == true
////////////////////////////////////////////////////////////////////////////////
template <typename, template <class...> class>
struct is_template_instance : public std::false_type {};

template <typename... Ts, template <class...> class T>
struct is_template_instance<T<Ts...>, T> : public std::true_type {};

template <template <class...> class T, typename U>
constexpr bool IsTemplateInstance(Type<U>) {
  return is_template_instance<U, T>::value;
}

}  // namespace htls::meta

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_H_
