#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_UTILS_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_UTILS_H_

#include <string_view>

#include <absl/strings/str_format.h>
#include "haversack/internal/haversack_impl.h"
#include "meta/type.h"
#include "meta/type_set.h"

namespace hotels::haversack {

template <typename HaversackT, typename... DirectDeps>
struct StreamHaversack {
  template <typename Sink>
  friend void AbslStringify(Sink& sink, const StreamHaversack&) {
    auto print_one = [&](auto child_haversack, auto caller, auto parent) {
      if (caller == htls::meta::type_c<void>) {
        absl::Format(&sink, R"("%s" -> "%s"
)",
                     htls::meta::DebugTypeName(parent),
                     htls::meta::DebugTypeName(child_haversack));
      } else {
        std::string_view caller_name = htls::meta::DebugTypeName(caller);
        caller_name.remove_prefix(sizeof("hotels::haversack::Calls<") - 1);
        caller_name.remove_suffix(sizeof(">") - 1);
        absl::Format(&sink, R"("%s" -> "%s" -> "%s"
)",
                     htls::meta::DebugTypeName(parent), caller_name,
                     htls::meta::DebugTypeName(child_haversack));
      }
      htls::meta::TypeSet direct_deps =
          htls::meta::MakeTypeSet(htls::meta::type_c<DirectDeps>...) &
          internal::TraitsOf(child_haversack).direct;
      htls::meta::Transform(
          [&](auto direct_dep) {
            absl::Format(&sink, R"("%s" -> "%s"
)",
                         htls::meta::DebugTypeName(child_haversack),
                         htls::meta::DebugTypeName(direct_dep));
            return false;
          },
          direct_deps.Tuple());
    };
    print_one(htls::meta::type_c<HaversackT>, htls::meta::type_c<void>,
              htls::meta::type_c<void>);
    htls::meta::Transform(
        [&](auto child_haversack_metadata) {
          typename decltype(child_haversack_metadata)::type().Visit(
              print_one, htls::meta::type_c<HaversackT>);
          return false;
        },
        HaversackT::Traits().child_haversacks.Tuple());
  }
};

}  // namespace hotels::haversack

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_UTILS_H_
