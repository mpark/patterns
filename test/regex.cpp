// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

struct Token {
  enum Kind { ID, NUM, OP, WS };

  Token(Kind kind_, std::string lexeme_)
      : kind(kind_), lexeme(std::move(lexeme_)) {}

  Kind kind;
  std::string lexeme;
};

bool operator==(const Token &lhs, const Token &rhs) {
  return std::tie(lhs.kind, lhs.lexeme) == std::tie(rhs.kind, rhs.lexeme);
}

bool operator!=(const Token &lhs, const Token &rhs) { return !(lhs == rhs); }

TEST(Regex, String) {
  auto lex = [](std::string_view sv) {
    using namespace mpark::patterns;
    return match(sv)(
        pattern(arg(re.match(R"~([_a-zA-Z]\w*)~"))) = [](auto lexeme) {
          return Token(Token::ID, std::string(lexeme));
        },
        pattern(arg(re.match(R"~(-?\d+)~"))) = [](auto lexeme) {
          return Token(Token::NUM, std::string(lexeme));
        },
        pattern(arg(re.match(R"~([*|/|+|-])~"))) = [](auto lexeme) {
          return Token(Token::OP, std::string(lexeme));
        });
  };

  EXPECT_EQ(lex("foobar"), Token(Token::ID, "foobar"));
  EXPECT_EQ(lex("x"), Token(Token::ID, "x"));
  EXPECT_EQ(lex("x0"), Token(Token::ID, "x0"));

  EXPECT_EQ(lex("42"), Token(Token::NUM, "42"));
  EXPECT_EQ(lex("101"), Token(Token::NUM, "101"));
  EXPECT_EQ(lex("-202"), Token(Token::NUM, "-202"));

  EXPECT_EQ(lex("*"), Token(Token::OP, "*"));
  EXPECT_EQ(lex("/"), Token(Token::OP, "/"));
  EXPECT_EQ(lex("+"), Token(Token::OP, "+"));
  EXPECT_EQ(lex("-"), Token(Token::OP, "-"));
}

TEST(Regex, Stream) {
  std::regex id(R"~([_a-zA-Z]\w*)~");
  std::regex num(R"~(-?\d+)~");
  std::regex op(R"~([*|/|+|-])~");

  std::istringstream strm("foo + -42 - x * 101 / bar");

  std::vector<Token> expected = {{Token::ID, "foo"},
                                 {Token::OP, "+"},
                                 {Token::NUM, "-42"},
                                 {Token::OP, "-"},
                                 {Token::ID, "x"},
                                 {Token::OP, "*"},
                                 {Token::NUM, "101"},
                                 {Token::OP, "/"},
                                 {Token::ID, "bar"}};

  std::vector<Token> actual;
  for (std::string token; strm >> token; ) {
    using namespace mpark::patterns;
    actual.push_back(match(token)(
        pattern(arg(re.match(id))) = [](const auto &lexeme) {
          return Token(Token::ID, lexeme);
        },
        pattern(arg(re.match(num))) = [](const auto &lexeme) {
          return Token(Token::NUM, lexeme);
        },
        pattern(arg(re.match(op))) = [](const auto &lexeme) {
          return Token(Token::OP, lexeme);
        }));
  }

  EXPECT_EQ(expected, actual);
}
