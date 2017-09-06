package ru.spbau.bachelors2015.veselov.tokenization

import org.scalatest.FunSuite
import ru.spbau.bachelors2015.veselov.tokenization.tokens._

class ExpressionTokenizerImplTest extends FunSuite {
  test("tokenize single integer number") {
    testSingleNumber("1234")
  }

  test("tokenize single zero") {
    testSingleNumber("0")
  }

  test("tokenize single double number") {
    testSingleNumber("12.34")
  }

  test("nextToken on empty Tokenizer") {
    val string = "1234"
    val tokenizer = new ExpressionTokenizerImpl(string)
    consumeAllTokens(tokenizer, List(new NumberToken(string)))

    assertThrows[NoTokensLeftException] {
      tokenizer.nextToken()
    }
  }

  test("invalid string") {
    val tokenizer = new ExpressionTokenizerImpl("abacaba")

    assertThrows[InvalidTokenException] {
      tokenizer.nextToken()
    }
  }

  test("complicated expression with all possible types of tokens") {
    val tokenizer = new ExpressionTokenizerImpl("1+2-(3.3*4)/5.5")

    consumeAllTokens(tokenizer,
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

  private def consumeAllTokens(tokenizer: ExpressionTokenizerImpl,
                               expectedTokens: List[Token]): Unit = {
    for (token <- expectedTokens) {
      assert(token == tokenizer.nextToken())
    }

    assert(tokenizer.isEmpty())
  }

  private def testSingleNumber(string: String): Unit = {
    consumeAllTokens(new ExpressionTokenizerImpl(string), List(new NumberToken(string)))
  }
}
