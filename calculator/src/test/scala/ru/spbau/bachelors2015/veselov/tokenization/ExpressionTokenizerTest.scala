package ru.spbau.bachelors2015.veselov.tokenization

import org.scalatest.FunSuite

class ExpressionTokenizerTest extends FunSuite {
  test("tokenize single integer number") {
    testSingleNumber("1234")
  }

  test("tokenize single zero") {
    testSingleNumber("0")
  }

  test("tokenize single double number") {
    testSingleNumber("12.34")
  }

  test("invalid string") {
    assertThrows[InvalidTokenException] {
      ExpressionTokenizer.tokenList("abacaba")
    }
  }

  test("complicated expression with all possible types of tokens") {
    testCharSequence("1   + 2 - (3.3 * 4) / 5.5",
      List(
        new Token(TokenType.Number, "1"),
        new Token(TokenType.Whitespace, "   "),
        new Token(TokenType.AddOp, "+"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.Number, "2"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.SubOp, "-"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.LeftParen, "("),
        new Token(TokenType.Number, "3.3"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.MulOp, "*"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.Number, "4"),
        new Token(TokenType.RightParen, ")"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.DivOp, "/"),
        new Token(TokenType.Whitespace, " "),
        new Token(TokenType.Number, "5.5")))
  }

  private def testCharSequence(chars: CharSequence,
                               expectedTokens: List[Token]): Unit = {
    assert(expectedTokens == ExpressionTokenizer.tokenList(chars))
  }

  private def testSingleNumber(string: String): Unit = {
    testCharSequence(string, List(new Token(TokenType.Number, string)))
  }
}
