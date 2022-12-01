#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_META_FUNCTION_TRAITS_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_META_FUNCTION_TRAITS_H_

#include <iosfwd>
#include <tuple>
namespace htls::meta {

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
  static constexpr std::size_t arity = sizeof...(Args);
  using result_type = R;
  template <std::size_t i>
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

}  // namespace htls::meta

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_META_FUNCTION_TRAITS_H_
