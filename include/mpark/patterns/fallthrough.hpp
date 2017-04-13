// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_FALLTHROUGH_HPP
#define MPARK_PATTERNS_FALLTHROUGH_HPP

namespace mpark {

  namespace patterns { namespace detail { struct FallThrough {}; } }

  [[noreturn]] void fallthrough() { throw patterns::detail::FallThrough{}; }
  void when(bool condition) { if (!condition) { fallthrough(); } }

}  // namespace mpark

#endif  // MPARK_PATTERNS_FALLTHROUGH_HPP
