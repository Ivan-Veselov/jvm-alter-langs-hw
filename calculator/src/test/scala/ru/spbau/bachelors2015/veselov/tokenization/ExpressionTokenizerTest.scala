package ru.spbau.bachelors2015.veselov.tokenization

import org.scalatest.FunSuite
import ru.spbau.bachelors2015.veselov.tokenization.tokens._

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
    testCharSequence("1+2-(3.3*4)/5.5",
      List(
        new NumberToken("1"),
        new AddOpToken,
        new NumberToken("2"),
        new SubOpToken,
        new LeftParenToken,
        new NumberToken("3.3"),
        new MulOpToken,
        new NumberToken("4"),
        new RightParenToken,
        new DivOpToken,
        new NumberToken("5.5")))
  }

  private def testCharSequence(chars: CharSequence,
                               expectedTokens: List[Token]): Unit = {
    assert(expectedTokens == ExpressionTokenizer.tokenList(chars))
  }

  private def testSingleNumber(string: String): Unit = {
    testCharSequence(string, List(new NumberToken(string)))
  }
}
