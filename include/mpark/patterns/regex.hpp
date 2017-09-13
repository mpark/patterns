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

  /* inline */ constexpr struct SubMatches {} sub_matches{};

  /*
  template <typename... Patterns>
  struct Groups { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto groups(const Patterns &... patterns) noexcept {
    return Groups<Patterns...>{std::tie(patterns...)};
  }
  */

  template <typename RegexF, typename = void>
  struct Regex { std::regex regex; };

  template <typename RegexF>
  struct Regex<RegexF, SubMatches> { std::regex regex; };

  /*
  template <typename RegexF, typename... Patterns>
  struct Regex<RegexF, Groups<Patterns...>> {
    std::regex regex;
    const Groups<Patterns...> &groups;
  };
  */

  struct Re {
    auto match(std::regex regex) const {
      return Regex<RegexMatch>{std::move(regex)};
    }

    auto match(std::regex regex, SubMatches) const {
      return Regex<RegexMatch, SubMatches>{std::move(regex)};
    }

    /*
    template <typename... Patterns>
    auto match(std::regex regex, const Groups<Patterns...> &groups) const {
      return Regex<RegexMatch, Groups<Patterns...>>{std::move(regex), groups};
    }
    */

    template <typename... Args>
    auto match(std::string_view sv, Args &&... args) const {
      return match(std::regex(std::begin(sv), std::end(sv)),
                   std::forward<Args>(args)...);
    }

    auto search(std::regex regex) const {
      return Regex<RegexSearch>{std::move(regex)};
    }

    auto search(std::regex regex, SubMatches) const {
      return Regex<RegexSearch, SubMatches>{std::move(regex)};
    }

    /*
    template <typename... Patterns>
    auto search(std::regex regex, const Groups<Patterns...> &groups) const {
      return Regex<RegexSearch, Groups<Patterns...>>{std::move(regex), groups};
    }
    */

    template <typename... Args>
    auto search(std::string_view sv, Args &&... args) const {
      return search(std::regex(std::begin(sv), std::end(sv)),
                    std::forward<Args>(args)...);
    }
  };

  /* inline */ constexpr Re re{};

  template <typename RegexF, typename F>
  auto matches(const Regex<RegexF> &r, const char *s, F &&f) {
    return RegexF{}(s, r.regex) ? match_invoke(std::forward<F>(f)) : no_match;
  }

  template <typename RegexF,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF> &r, Value &&value, F &&f) {
    return RegexF{}(std::cbegin(value), std::cend(value), r.regex)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

  template <typename RegexF, typename F>
  auto matches(const Regex<RegexF, SubMatches> &r, const char *s, F &&f) {
    std::cmatch results;
    return RegexF{}(s, results, r.regex)
               ? match_invoke(std::forward<F>(f), std::move(results))
               : no_match;
  }

  template <typename RegexF,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF, SubMatches> &r, Value &&value, F &&f) {
    std::match_results<typename std::decay_t<Value>::const_iterator> results;
    return RegexF{}(std::cbegin(value), std::cend(value), results, r.regex)
               ? match_invoke(std::forward<F>(f), std::move(results))
               : no_match;
  }

  /*
  namespace detail {

    template <typename BiIter, std::size_t... Is>
    auto as_tuple(std::match_results<BiIter> &&results,
                  std::index_sequence<Is...>) {
      // Ignore the first element which is the full match.
      return std::make_tuple(std::move(results)[Is + 1]...);
    }

  }  // namespace detail

  template <typename RegexF, typename... Patterns, typename F>
  auto matches(const Regex<RegexF, Groups<Patterns...>> &r,
               const char *s,
               F &&f) {
    std::cmatch results;
    return RegexF{}(s, results, r.regex)
               ? match(Prod{r.groups.patterns},
                       as_tuple(std::move(results),
                                std::index_sequence_for<Patterns...>{}),
                       std::forward<F>(f))
               : no_match;
  }

  template <typename RegexF,
            typename... Patterns,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF, Groups<Patterns...>> &r,
               Value &&value,
               F &&f) {
    std::match_results<typename std::decay_t<Value>::const_iterator> results;
    return RegexF{}(std::cbegin(value), std::cend(value), results, r.regex)
               ? match(Prod{r.groups.patterns},
                       as_tuple(std::move(results),
                                std::index_sequence_for<Patterns...>{}),
                       std::forward<F>(f))
               : no_match;
  }
  */

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_REGEX_HPP
