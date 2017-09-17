// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <cstddef>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>

#include <gtest/gtest.h>

namespace memory {

  template <typename T>
  struct part_ptr : std::unique_ptr<T> {
    using super = std::unique_ptr<T>;
    using super::super;

    T &operator*() & { return *this->get(); }
    const T &operator*() const & { return *this->get(); }
    T &&operator*() && { return std::move(*this->get()); }
    const T &&operator*() const && { return std::move(*this->get()); }
  };

  template <typename T, typename... Args>
  auto make_part(Args&&... args) {
    return part_ptr<T>(new T(std::forward<Args>(args)...));
  }

}  // namespace memory

namespace utility {

  template <typename Arg>
  std::string stringify(Arg &&arg) {
    std::ostringstream strm;
    strm << std::forward<Arg>(arg);
    return strm.str();
  }

}  // namespace utility

namespace rb {

  enum Color { Red, Black };

  template <typename T> class Node;

  template <typename T> struct is_node : std::false_type {};
  template <typename T> struct is_node<Node<T>> : std::true_type {};

  template <typename T>
  class Node {
    public:
    explicit Node(Color color,
                  memory::part_ptr<Node> &&lhs,
                  T t,
                  memory::part_ptr<Node> &&rhs)
        : color_(color),
          lhs_(std::move(lhs)),
          t_(std::move(t)),
          rhs_(std::move(rhs)) {}

    void balance();

    private:
    Color color_;
    memory::part_ptr<Node> lhs_;
    T t_;
    memory::part_ptr<Node> rhs_;

    friend std::ostream &operator<<(std::ostream &strm, const Node &node) {
      const auto & [color, lhs, value, rhs] = node;
      strm << '{' << (color ? "Black" : "Red") << ',';
      if (lhs) {
        strm << *lhs;
      } else {
        strm << "null";
      }
      strm << ',' << value << ',';
      if (rhs) {
        strm << *rhs;
      } else {
        strm << "null";
      }
      strm << '}';
      return strm;
    }

    template <std::size_t I,
              typename FwdNode,
              std::enable_if_t<is_node<std::decay_t<FwdNode>>::value, int> = 0>
    friend auto &&get(FwdNode &&node) noexcept {
      if      constexpr (I == 0) return std::forward<FwdNode>(node).color_;
      else if constexpr (I == 1) return std::forward<FwdNode>(node).lhs_;
      else if constexpr (I == 2) return std::forward<FwdNode>(node).t_;
      else if constexpr (I == 3) return std::forward<FwdNode>(node).rhs_;
    }
  };

  template <typename T>
  void Node<T>::balance() {
    using namespace mpark::patterns;
    IDENTIFIERS(a, b, c, d, x, y, z);
    match(std::move(*this))(
        pattern(
            anyof(ds(Black, some(ds(Red, some(ds(Red, a, x, b)), y, c)), z, d),
                  ds(Black, some(ds(Red, a, x, some(ds(Red, b, y, c)))), z, d),
                  ds(Black, a, x, some(ds(Red, some(ds(Red, b, y, c)), z, d))),
                  ds(Black, a, x, some(ds(Red, b, y, some(ds(Red, c, z, d))))))) =
            [&](auto &&a, auto &&x, auto &&b, auto &&y, auto &&c, auto &&z, auto &&d) {
#define FWD(x) std::forward<decltype(x)>(x)

              *this = Node(Red,
                           memory::make_part<Node>(Black, FWD(a), FWD(x), FWD(b)),
                           y,
                           memory::make_part<Node>(Black, FWD(c), FWD(z), FWD(d)));
#undef FWD
            },
        pattern(_) = [] {});
  }

}  // namespace rb

namespace std {

  template <typename T>
  class tuple_size<rb::Node<T>> : public integral_constant<size_t, 4> {};

  template <typename T>
  class tuple_element<0, rb::Node<T>> { public: using type = rb::Color; };

  template <typename T>
  class tuple_element<1, rb::Node<T>> {
    public:
    using type = memory::part_ptr<rb::Node<T>>;
  };

  template <typename T>
  class tuple_element<2, rb::Node<T>> { public: using type = T; };

  template <typename T>
  class tuple_element<3, rb::Node<T>> {
    public:
    using type = memory::part_ptr<rb::Node<T>>;
  };

}  // namespace std

TEST(Balance, Typical) {
  auto tree = memory::make_part<rb::Node<int>>(
      rb::Black,
      memory::make_part<rb::Node<int>>(
          rb::Red,
          memory::make_part<rb::Node<int>>(rb::Red, nullptr, 101, nullptr),
          202,
          nullptr),
      303,
      nullptr);

  std::string before = "{Black,{Red,{Red,null,101,null},202,null},303,null}";
  EXPECT_EQ(before, utility::stringify(*tree));

  tree->balance();

  std::string after = "{Red,{Black,null,101,null},202,{Black,null,303,null}}";
  EXPECT_EQ(after, utility::stringify(*tree));
}
