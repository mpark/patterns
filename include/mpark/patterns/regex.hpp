// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_REGEX_HPP
#define MPARK_PATTERNS_REGEX_HPP

#include <cstddef>
#include <iterator>
#include <regex>
#include <string_view>
#include <tuple>
#include <utility>

namespace mpark::patterns {

  struct RegexMatch {
    template <typename... Args>
    bool operator()(Args &&... args) const {
      return std::regex_match(std::forward<Args>(args)...);
    }
  };

  struct RegexSearch {
    template <typename... Args>
    bool operator()(Args &&... args) const {
      return std::regex_search(std::forward<Args>(args)...);
    }
  };

  template <typename RegexF>
  struct Regex { std::regex regex; };

  struct Re {
    auto match(std::regex regex) const {
      return Regex<RegexMatch>{std::move(regex)};
    }

    template <typename... Args>
    auto match(std::string_view sv, Args &&... args) const {
      return match(std::regex(std::begin(sv), std::end(sv)),
                   std::forward<Args>(args)...);
    }

    auto search(std::regex regex) const {
      return Regex<RegexSearch>{std::move(regex)};
    }

    template <typename... Args>
    auto search(std::string_view sv, Args &&... args) const {
      return search(std::regex(std::begin(sv), std::end(sv)),
                    std::forward<Args>(args)...);
    }
  };

  inline constexpr Re re{};

  template <typename RegexF, typename F>
  auto try_match(const Regex<RegexF> &r, const char *s, F &&f) {
    return RegexF{}(s, r.regex) ? match_invoke(std::forward<F>(f)) : no_match;
  }

  template <typename RegexF,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto try_match(const Regex<RegexF> &r, Value &&value, F &&f) {
    return RegexF{}(std::cbegin(value), std::cend(value), r.regex)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_REGEX_HPP
