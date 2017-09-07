package ru.spbau.bachelors2015.veselov.tokenization

import scala.collection.mutable.ListBuffer

/**
  * A singleton object which can create a list of arithmetical tokens from a char sequence.
  */
object ExpressionTokenizer {
  /**
    * Creates a list of arithmetical tokens from a given char sequence.
    *
    * @param chars a characters which should be tokenized.
    * @return a resulting list of tokens.
    */
  def tokenList(chars: CharSequence): List[Token] = {
    val state = new InnerState(chars)
    val list = new ListBuffer[Token]

    while (state.hasLeft()) {
      list.+=(state.nextToken())
    }

    return list.toList
  }

  private class InnerState(var chars: CharSequence) {
    private val pattern = TokenType.values.foldLeft(new StringBuilder)(
      (result, elem) => {
        if (result.nonEmpty) {
          result += '|'
        }

        result ++= "(?<" + elem.toString + ">" + TokenType.regex(elem) + ")"
      }
    ).toString().r

    def hasLeft(): Boolean = chars.length() > 0

    def nextToken(): Token = {
      val option = pattern.findPrefixMatchOf(chars)
      if (option.isEmpty) {
        throw new InvalidTokenException
      }

      val matchResult = option.get
      val matched = matchResult.matched

      chars = chars.subSequence(matchResult.end, chars.length)

      for (tokenType <- TokenType.values) {
        if (option.get.group(tokenType.toString) != null) {
          return new Token(tokenType, matched)
        }
      }

      throw new TokenizationError
    }
  }
}