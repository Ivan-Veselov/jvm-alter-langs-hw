package ru.spbau.bachelors2015.veselov.tokenization

import org.scalatest.FunSuite

class TokenizerTest extends FunSuite {
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
    val tokenizer = new Tokenizer(string)
    consumeTokens(tokenizer, List(new NumberToken(string.toDouble)))

    assertThrows[NoTokensLeftException] {
      tokenizer.nextToken()
    }
  }

  test("invalid string") {
    val tokenizer = new Tokenizer("abacaba")

    assertThrows[InvalidTokenException] {
      tokenizer.nextToken()
    }
  }

  private def consumeTokens(tokenizer: Tokenizer, expectedTokens: List[Token]): Unit = {
    for (token <- expectedTokens) {
      assert(token == tokenizer.nextToken())
    }

    assert(tokenizer.isEmpty())
  }

  private def testSingleNumber(string: String): Unit = {
    consumeTokens(new Tokenizer(string), List(new NumberToken(string.toDouble)))
  }
}
