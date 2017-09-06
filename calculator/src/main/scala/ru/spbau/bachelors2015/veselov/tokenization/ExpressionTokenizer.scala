package ru.spbau.bachelors2015.veselov.tokenization

import ru.spbau.bachelors2015.veselov.tokenization.tokens._

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

  // TODO: build pattern automatically
  private class InnerState(var chars: CharSequence) {
    private val pattern = (raw"(?<number>(0|([1-9]\d*))(\.\d+)?)|" +
                           raw"(?<addop>\+)|" +
                           raw"(?<subop>-)|" +
                           raw"(?<mulop>\*)|" +
                           raw"(?<divop>/)|" +
                           raw"(?<leftparen>\()|" +
                           raw"(?<rightparen>\))").r

    def hasLeft(): Boolean = chars.length() > 0

    def nextToken(): Token = {
      val option = pattern.findPrefixMatchOf(chars)
      if (option.isEmpty) {
        throw new InvalidTokenException
      }

      val matchResult = option.get
      val matched = matchResult.matched

      chars = chars.subSequence(matchResult.end, chars.length)

      if (option.get.group("number") != null) {
        return new NumberToken(matched)
      }

      if (option.get.group("addop") != null) {
        return new AddOpToken
      }

      if (option.get.group("subop") != null) {
        return new SubOpToken
      }

      if (option.get.group("mulop") != null) {
        return new MulOpToken
      }

      if (option.get.group("divop") != null) {
        return new DivOpToken
      }

      if (option.get.group("leftparen") != null) {
        return new LeftParenToken
      }

      if (option.get.group("rightparen") != null) {
        return new RightParenToken
      }

      throw new TokenizationError
    }
  }
}