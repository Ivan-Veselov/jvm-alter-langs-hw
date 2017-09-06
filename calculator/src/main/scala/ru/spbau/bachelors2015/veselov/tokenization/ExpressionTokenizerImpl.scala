package ru.spbau.bachelors2015.veselov.tokenization

import ru.spbau.bachelors2015.veselov.tokenization.tokens._

// TODO: build pattern automatically

/**
  * ExpressionTokenizer breaks a given string into a sequence of arithmetical tokens.
  *
  * @param string a string which should be tokenized.
  */
class ExpressionTokenizerImpl(string: String) extends ExpressionTokenizer {
  private var chars: CharSequence = string

  private val pattern = (raw"(?<number>(0|([1-9]\d*))(\.\d+)?)|" +
                         raw"(?<addop>\+)|" +
                         raw"(?<subop>-)|" +
                         raw"(?<mulop>\*)|" +
                         raw"(?<divop>/)|" +
                         raw"(?<leftparen>\()|" +
                         raw"(?<rightparen>\))").r

  /**
    * Checks weather the string is consumed.
    */
  override def isEmpty(): Boolean = chars.length == 0

  /**
    * Returns next token in the sequence.
    */
  override def nextToken(): Token = {
    if (isEmpty()) {
      throw new NoTokensLeftException
    }

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