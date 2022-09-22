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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_UTIL_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_UTIL_H_

#include <string_view>
#include <tuple>
#include <type_traits>

namespace hotels::haversack::internal {

////////////////////////////////////////////////////////////////////////////////
// Check if the first template argument is an template instance of the second
// argument; e.g.
//   is_template_instance_v<std::optional<int>, std::optional> == true
////////////////////////////////////////////////////////////////////////////////
template <typename, template <class...> class>
struct is_template_instance : public std::false_type {};

template <typename... Ts, template <class...> class T>
struct is_template_instance<T<Ts...>, T> : public std::true_type {};

template <typename T, template <class...> class U>
inline constexpr bool is_template_instance_v =
    is_template_instance<T, U>::value;

////////////////////////////////////////////////////////////////////////////////
// Traits for callables.
//
// Supported traits:
//
//   Return type:
//     function_traits<F>::result_type
//
//   Argument type:
//    function_traits<F>::arg<N>
//
//   Argument types as tuple
//     function_traits<F>::args
//
//   Number of arguments:
//    function_traits<F>::arity
//
// Supports all callables: Function, function pointer, functor, lambda, pointer
// to member function, and pointer to member variable.
//
// The implicit "this" argument in pointer to member functions and variables is
// not included in the arity or arguments.
////////////////////////////////////////////////////////////////////////////////

template <typename>
struct as_const_signature;

template <typename T>
using as_const_signature_t = typename as_const_signature<T>::type;

namespace internal_function_traits {

template <typename, bool>
struct function_traits_impl;

template <typename R, typename... Args, bool IsConst>
struct function_traits_impl<R(Args...), IsConst> {
  static constexpr size_t arity = sizeof...(Args);
  using result_type = R;
  template <size_t i>
  using arg = typename std::tuple_element<i, std::tuple<Args...>>::type;
  using args = std::tuple<Args...>;
  using signature =
      std::conditional_t<IsConst, as_const_signature_t<R(Args...)>, R(Args...)>;
};

}  // namespace internal_function_traits

template <typename, typename = void>
struct function_traits;

// Specialization for function.
template <typename R, typename... Args>
struct function_traits<R(Args...)>
    : internal_function_traits::function_traits_impl<R(Args...), false> {};

// Specialization for const function.
template <typename R, typename... Args>
struct function_traits<R(Args...) const>
    : internal_function_traits::function_traits_impl<R(Args...), true> {};

// Specialization for function pointer.
template <typename R, typename... Args>
struct function_traits<R (*)(Args...)> : public function_traits<R(Args...)> {};

// Specialization for pointer to const member function.
template <typename C, typename R, typename... Args>
struct function_traits<R (C::*)(Args...) const>
    : public function_traits<R(Args...) const> {
  using member_of = C;
};

// Specialization for pointer to non-const member function.
template <typename C, typename R, typename... Args>
struct function_traits<R (C::*)(Args...)> : public function_traits<R(Args...)> {
  using member_of = C;
};

// Specialization for pointer to member variable.
template <typename C, typename R>
struct function_traits<R C::*> : public function_traits<R (C::*)()> {
  using member_of = C;
};

// Base template for functors and lambdas.
template <typename F>
struct function_traits<F, std::void_t<decltype(&F::operator())>>
    : public function_traits<decltype(&F::operator())> {};

// Specialization for lvalue refs.
template <typename F>
struct function_traits<F&, std::void_t<decltype(&F::operator())>>
    : public function_traits<F> {};

// Specialization for rvalue refs.
template <typename F>
struct function_traits<F&&, std::void_t<decltype(&F::operator())>>
    : public function_traits<F> {};

template <typename R, typename... Args>
struct as_const_signature<R(Args...)> {
  using type = R(Args...) const;
};

template <typename R, typename... Args>
struct as_const_signature<R(Args...) const> {
  using type = R(Args...) const;
};

////////////////////////////////////////////////////////////////////////////////
// debug_type_name_v<T> is a human readable string of the type name of T. This
// helper should only be used for debugging and not for RTTI in production (or
// even testing).
//
// Example:
//   debug_type_name_v<std::vector<int>> == "std::vector<int>"
////////////////////////////////////////////////////////////////////////////////

namespace internal_meta_util {
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
  std::string_view raw_name = internal_meta_util::GetRawTypeName<T>();
  raw_name.remove_prefix(internal_meta_util::type_name_prefix);
  raw_name.remove_suffix(internal_meta_util::type_name_suffix);
  return raw_name;
}
}  // namespace internal_meta_util

template <typename T>
inline constexpr std::string_view debug_type_name_v =
    internal_meta_util::GetDebugTypeName<T>();

namespace internal_assert_is {

enum class Placeholder : int;

template <std::size_t N, std::size_t M, typename Compare,
          typename... ContextPtrs>
constexpr void assert_is_impl(Compare cmp, ContextPtrs*...) {
  static_assert(cmp(N, M));
}

template <std::size_t N, std::size_t M, typename Context,
          typename... ContextRest, typename Compare, typename... ContextPtrs>
constexpr void assert_is_helper(Compare cmp, ContextPtrs*... context_ptrs) {
  if constexpr (sizeof...(ContextRest) == 0) {
    assert_is_impl<N, M>(cmp, context_ptrs..., static_cast<Context*>(nullptr),
                         static_cast<Placeholder*>(nullptr));
  } else {
    assert_is_helper<N, M, ContextRest...>(cmp, context_ptrs...,
                                           static_cast<Context*>(nullptr),
                                           static_cast<Placeholder*>(nullptr));
  }
}

}  // namespace internal_assert_is

// Perform a static_assert on N and M using cmp. The list of Context types will
// be interleaved with Placeholder for easy parsing.
template <std::size_t N, std::size_t M, typename... Context, typename Compare>
constexpr void assert_is(Compare cmp) {
  internal_assert_is::assert_is_helper<N, M, Context...>(
      cmp, static_cast<internal_assert_is::Placeholder*>(nullptr));
}

}  // namespace hotels::haversack::internal

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_UTIL_H_
