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

TEST(Regex, NoSubMatches_String) {
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

TEST(Regex, NoSubMatches_Stream) {
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

TEST(Regex, SubMatches_String) {
  std::regex hex(R"~(#?([a-f0-9]{6}|[a-f0-9]{3}))~");
  std::regex email(
      R"~(([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6}))~");

  auto test = [&](std::string_view sv) {
    using namespace mpark::patterns;
    return match(sv)(pattern(anyof(re.match(hex, sub_matches),
                                   re.match(email, sub_matches))) =
                         [](std::cmatch &&results) {
                           std::vector<std::string> result;
                           for (std::size_t i = 1; i < results.size(); ++i) {
                             result.emplace_back(results[i]);
                           }
                           return result;
                         });
  };

  EXPECT_EQ(std::vector<std::string>({"a3c113"}), test("#a3c113"));
  EXPECT_EQ(std::vector<std::string>({"mcypark", "gmail", "com"}),
            test("mcypark@gmail.com"));
}

TEST(Regex, Captures_Stream) {
  std::regex id(R"~(^[_a-zA-Z]\w*)~");
  std::regex num(R"~(^-?\d+)~");
  std::regex op(R"~(^[*|/|+|-])~");
  std::regex ws(R"~(^\s+)~");

  std::string s("foo +\t-42  - x \n\n * 101 / bar");

  std::vector<Token> expected = {{Token::ID, "foo"},
                                 {Token::WS, " "},
                                 {Token::OP, "+"},
                                 {Token::WS, "\t"},
                                 {Token::NUM, "-42"},
                                 {Token::WS, "  "},
                                 {Token::OP, "-"},
                                 {Token::WS, " "},
                                 {Token::ID, "x"},
                                 {Token::WS, " \n\n "},
                                 {Token::OP, "*"},
                                 {Token::WS, " "},
                                 {Token::NUM, "101"},
                                 {Token::WS, " "},
                                 {Token::OP, "/"},
                                 {Token::WS, " "},
                                 {Token::ID, "bar"}};

  std::vector<Token> actual;
  while (!s.empty()) {
    using namespace mpark::patterns;
    Token token = match(s)(
        pattern(re.search(id, sub_matches)) = [&s](std::smatch &&results) {
          std::string lexeme = std::move(results)[0];
          s = results.suffix();
          return Token(Token::ID, std::move(lexeme));
        },
        pattern(re.search(num, sub_matches)) = [&s](std::smatch &&results) {
          std::string lexeme = std::move(results)[0];
          s = results.suffix();
          return Token(Token::NUM, std::move(lexeme));
        },
        pattern(re.search(op, sub_matches)) = [&s](std::smatch &&results) {
          std::string lexeme = std::move(results)[0];
          s = results.suffix();
          return Token(Token::OP, std::move(lexeme));
        },
        pattern(re.search(ws, sub_matches)) = [&s](std::smatch &&results) {
          std::string lexeme = std::move(results)[0];
          s = results.suffix();
          return Token(Token::WS, std::move(lexeme));
        });

    actual.push_back(std::move(token));
  }

  EXPECT_EQ(expected, actual);
}
