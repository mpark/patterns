// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_LET_HPP
#define MPARK_PATTERNS_LET_HPP

#include <utility>

namespace mpark::patterns {

  template <typename Pattern, typename Value>
  struct IfLet {
    template <typename F>
    void operator=(F &&f) && noexcept {
      match<void>(std::move(case_).rhs())(
          std::move(case_).pattern = std::forward<F>(f),
          pattern(_) = [] {});
    }

    detail::Case<Pattern, Value> &&case_;
  };

  template <typename Pattern, typename Value>
  auto if_let(detail::Case<Pattern, Value> &&case_) noexcept {
    static_assert(Pattern::size <= 1,
                  "The `if_let` statement cannot have more than 1 pattern "
                  "since it only matches a single value. If you're trying to "
                  "match a destructurable type, use the `ds` pattern!");
    return IfLet<Pattern, Value>{std::move(case_)};
  }

  enum ControlFlow { Break, Continue };

  template <typename Pattern, typename Value>
  struct ForLet {
    template <typename F>
    void operator=(F &&f) && noexcept {
      for (auto&& elem : std::move(case_).rhs()) {
        ControlFlow control_flow = match(std::forward<decltype(elem)>(elem))(
            std::move(case_).pattern = [&](auto &&... args) {
              using R = lib::invoke_result_t<F, decltype(args)...>;
              if constexpr (std::is_same_v<R, ControlFlow>) {
                return std::invoke(std::forward<F>(f),
                                   std::forward<decltype(args)>(args)...);
              } else {
                std::invoke(std::forward<F>(f),
                            std::forward<decltype(args)>(args)...);
                return Continue;
              }
            },
            pattern(_) = [] { return Continue; });
        if (control_flow == Break) {
          break;
        } else if (control_flow == Continue) {
          continue;
        }
      }
    }

    detail::Case<Pattern, Value> &&case_;
  };

  template <typename Pattern, typename Value>
  auto for_let(detail::Case<Pattern, Value> &&case_) noexcept {
    static_assert(Pattern::size <= 1,
                  "The `for_let` statement cannot have more than 1 pattern "
                  "since it only matches a single value. If you're trying to "
                  "match a destructurable type, use the `ds` pattern!");
    return ForLet<Pattern, Value>{std::move(case_)};
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_LET_HPP
