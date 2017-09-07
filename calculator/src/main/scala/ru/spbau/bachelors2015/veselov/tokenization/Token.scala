package ru.spbau.bachelors2015.veselov.tokenization

import ru.spbau.bachelors2015.veselov.tokenization.TokenType.TokenType

/**
  * A token which consist of token type and string of actual characters.
  *
  * @param tokenType a type of this token.
  * @param token characters which correspond to this token.
  */
class Token(val tokenType: TokenType, val token: String) {
  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[Token]) {
      return false
    }

    val other = obj.asInstanceOf[Token]
    return tokenType == other.tokenType && token == other.token
  }
}
