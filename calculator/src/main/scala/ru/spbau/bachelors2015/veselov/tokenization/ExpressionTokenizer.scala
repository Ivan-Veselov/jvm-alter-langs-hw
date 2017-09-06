package ru.spbau.bachelors2015.veselov.tokenization

import ru.spbau.bachelors2015.veselov.tokenization.tokens.Token

/**
  * ExpressionTokenizer breaks a given string into a sequence of arithmetical tokens.
  */
trait ExpressionTokenizer {
  /**
    * Checks weather the string is consumed.
    */
  def isEmpty(): Boolean

  /**
    * Returns next token in the sequence.
    */
  def nextToken(): Token
}
