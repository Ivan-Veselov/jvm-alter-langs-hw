package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.Token

/**
  * Viewer provides simplified iterator functionality, but allows to have currently viewed element.
  *
  * @param tokens a list of tokens to view.
  */
class TokenViewer(tokens: List[Token]) {
  private val iterator = tokens.iterator

  private var token: Token = {
    if (iterator.isEmpty) {
      throw new EmptyExpressionException
    } else {
      iterator.next()
    }
  }

  /**
    * Returns true if there are some tokens left.
    */
  def nonEmpty(): Boolean = token != null

  /**
    * Returns currently viewed element.
    */
  def currentToken(): Token = {
    if (token == null) {
      throw new NoSuchElementException
    }

    return token
  }

  /**
    * Moves viewer to the next element.
    */
  def move(): Unit = {
    if (iterator.isEmpty) {
      token = null
    } else {
      token = iterator.next()
    }
  }
}
