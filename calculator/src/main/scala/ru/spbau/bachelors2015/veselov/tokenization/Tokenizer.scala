package ru.spbau.bachelors2015.veselov.tokenization

/**
  * Tokenizer breaks a given string into a sequence of tokens.
  *
  * @param string a string which should be tokenized.
  */
class Tokenizer(string: String) {
  private var chars: CharSequence = string

  private val pattern = raw"(?<number>(0|([1-9]\d*))(.\d+)?)".r

  /**
    * Checks weather the string is consumed.
    */
  def isEmpty(): Boolean = chars.length == 0

  /**
    * Returns next token in the sequence.
    */
  def nextToken(): Token = {
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
      return new NumberToken(matched.toDouble)
    }

    throw new TokenizationError
  }
}