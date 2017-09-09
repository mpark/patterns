// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_REGEX_HPP
#define MPARK_PATTERNS_REGEX_HPP

#include <iterator>
#include <regex>
#include <string_view>
#include <tuple>
#include <utility>

namespace mpark {
  namespace patterns {

    namespace detail {

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

    }  // namespace detail

    template <typename Op, typename... Patterns>
    struct Regex {
      explicit Regex(std::regex &&r, Prod<Patterns...> &&p)
          : regex(std::move(r)), prod(std::move(p)) {}

      std::regex regex;
      Prod<Patterns...> prod;
    };

    struct Re {
      template <typename... Patterns>
      auto match(std::string_view sv, const Patterns &... patterns) const {
        return Regex<detail::RegexMatch, Patterns...>(
            std::regex(std::begin(sv), std::end(sv)), prod(patterns...));
      }

      template <typename... Patterns>
      auto match(std::regex regex, const Patterns &... patterns) const {
        return Regex<detail::RegexMatch, Patterns...>(std::move(regex),
                                                      prod(patterns...));
      }

      template <typename... Patterns>
      auto search(std::string_view sv, const Patterns &... patterns) const {
        return Regex<detail::RegexSearch, Patterns...>(
            std::regex(std::begin(sv), std::end(sv)), prod(patterns...));
      }

      template <typename... Patterns>
      auto search(std::regex regex, const Patterns &... patterns) const {
        return Regex<detail::RegexSearch, Patterns...>(std::move(regex),
                                                       prod(patterns...));
      }
    };

    constexpr Re re{};

    namespace detail {

      template <typename BiIter, std::size_t... Is>
      auto as_string_views(std::match_results<BiIter> &&results,
                           std::index_sequence<Is...>) {
        return std::make_tuple([](std::sub_match<BiIter> sm) {
          return std::string_view(sm.first, std::distance(sm.first, sm.second));
        }(std::move(results)[Is + 1])...);
      }

    }  // namespace detail

    template <typename Op, typename... Patterns, typename F>
    auto matches(const Regex<Op, Patterns...> &r, const char *s, F &&f) {
      std::cmatch results;
      return Op{}(s, results, r.regex)
                 ? matches(r.prod,
                           detail::as_string_views(
                               std::move(results),
                               std::index_sequence_for<Patterns...>{}),
                           std::forward<F>(f))
                 : no_match;
    }

    template <typename Op,
              typename... Patterns,
              typename Value,
              typename F,
              std::enable_if_t<
                  !std::is_same<std::decay_t<Value>, const char *>::value,
                  int> = 0>
    auto matches(const Regex<Op, Patterns...> &r, Value &&value, F &&f) {
      std::match_results<typename std::decay_t<Value>::const_iterator> results;
      return Op{}(std::cbegin(value), std::cend(value), results, r.regex)
                 ? matches(r.prod,
                           detail::as_string_views(
                               std::move(results),
                               std::index_sequence_for<Patterns...>{}),
                           std::forward<F>(f))
                 : no_match;
    }

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_REGEX_HPP
