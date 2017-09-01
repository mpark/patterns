#include <iostream>
#include <memory>
#include <utility>
#include <tuple>

#include <mpark/match.hpp>

#define FWD(x) std::forward<decltype(x)>(x)

enum Color { Red, Black };

template <typename T>
struct Node : std::tuple<Color, std::shared_ptr<Node<T>>, T, std::shared_ptr<Node<T>>> {
  using super = std::tuple<Color, std::shared_ptr<Node<T>>, T, std::shared_ptr<Node<T>>>;
  using super::super;

  void balance();

  friend std::ostream& operator<<(std::ostream& strm, const Node& node) {
    const auto& [color, lhs, value, rhs] = node;
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
};

template <typename T>
void Node<T>::balance() {
  using namespace mpark;
  decltype(arg) a, b, c, d, x, y, z;
  match (*this) (
      pattern(
          anyof(prod(Black, some(prod(Red, some(prod(Red, a, x, b)), y, c)), z, d),
                prod(Black, some(prod(Red, a, x, some(prod(Red, b, y, c)))), z, d),
                prod(Black, a, x, some(prod(Red, some(prod(Red, b, y, c)), z, d))),
                prod(Black, a, x, some(prod(Red, b, y, some(prod(Red, c, z, d))))))) =
          [&](auto &&a, auto &&x, auto &&b, auto &&y, auto &&c, auto &&z, auto &&d) {
            *this = Node<T>{Red,
                            std::make_shared<Node>(Black, a, x, b),
                            y,
                            std::make_shared<Node>(Black, c, z, d)};
          },
      pattern(_) = [] {});
}

namespace std {
  template <typename T>
  class tuple_size<Node<T>> : public tuple_size<typename Node<T>::super> {};

  template <size_t I, typename T>
  class tuple_element<I, Node<T>> : public tuple_element<I, typename Node<T>::super> {};
}  // std

int main() {
  auto node = Node<int>{
      Black,
      std::make_shared<Node<int>>(
          Red,
          std::make_shared<Node<int>>(Red, nullptr, 101, nullptr),
          202,
          nullptr),
      303,
      nullptr};
  std::cout << node << '\n';
  node.balance();
  std::cout << node << '\n';
}
