// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <gtest/gtest.h>

struct Token {
  enum Kind { ID, NUM, OP };

  Token(Kind kind_, std::string lexeme_)
      : kind(kind_), lexeme(std::move(lexeme_)) {}

  Kind kind;
  std::string lexeme;
};

bool operator==(const Token &lhs, const Token &rhs) {
  return std::tie(lhs.kind, lhs.lexeme) == std::tie(rhs.kind, rhs.lexeme);
}

bool operator!=(const Token &lhs, const Token &rhs) { return !(lhs == rhs); }

auto tokenize(Token::Kind kind) {
  return [kind](std::string_view lexeme) {
    return Token(kind, std::string(lexeme));
  };
};

TEST(Patterns, NoCaptures_String) {
  auto lex = [](std::string_view s) {
    using namespace mpark::patterns;
    return match(s)(
        pattern(arg(re.match("[_a-zA-Z][_a-zA-Z0-9]*"))) = tokenize(Token::ID),
        pattern(arg(re.match("-?[0-9]+"))) = tokenize(Token::NUM),
        pattern(arg(re.match("[*|/|+|-]"))) = tokenize(Token::OP));
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

TEST(Patterns, NoCaptures_Stream) {
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

  using namespace mpark::patterns;

  auto id = re.match("[_a-zA-Z][_a-zA-Z0-9]*");
  auto num = re.match("-?[0-9]+");
  auto op = re.match("[*|/|+|-]");

  std::vector<Token> actual;
  for (std::string token; strm >> token; ) {
    actual.push_back(match(token)(pattern(arg(id)) = tokenize(Token::ID),
                                  pattern(arg(num)) = tokenize(Token::NUM),
                                  pattern(arg(op)) = tokenize(Token::OP)));
  }

  EXPECT_EQ(expected, actual);
}

TEST(Patterns, Captures_String) {
  std::regex hex("#?([a-f0-9]{6}|[a-f0-9]{3})");
  std::regex email(
      R"~(([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6}))~");

  auto test = [&](std::string_view s) {
    using namespace mpark::patterns;
    match(s)(
        pattern(re.match(hex, arg)) = [](auto code) {
          EXPECT_EQ("a3c113", code);
        },
        pattern(re.match(email, arg, arg, arg)) = [](
            auto name, auto domain, auto tld) {
          EXPECT_EQ("mcypark", name);
          EXPECT_EQ("gmail", domain);
          EXPECT_EQ("com", tld);
        });
  };

  test("#a3c113");
  test("mcypark@gmail.com");
}

// Use `re.search`.
TEST(Patterns, Captures_Stream) {
}
