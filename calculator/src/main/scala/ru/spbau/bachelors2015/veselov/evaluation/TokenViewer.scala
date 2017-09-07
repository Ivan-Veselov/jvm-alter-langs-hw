package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.Token

class TokenViewer(tokens: List[Token]) {
  private val iterator = tokens.iterator

  private var token: Token = {
    if (iterator.isEmpty) {
      throw new EmptyExpressionException
    } else {
      iterator.next()
    }
  }

  def nonEmpty(): Boolean = token != null

  def currentToken(): Token = {
    if (token == null) {
      throw new NoSuchElementException
    }

    return token
  }

  def move(): Unit = {
    if (iterator.isEmpty) {
      token = null
    } else {
      token = iterator.next()
    }
  }
}
