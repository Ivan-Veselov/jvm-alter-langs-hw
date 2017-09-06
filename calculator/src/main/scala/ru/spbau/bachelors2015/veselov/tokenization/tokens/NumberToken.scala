package ru.spbau.bachelors2015.veselov.tokenization.tokens

class NumberToken(val string: String) extends Token {
  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[NumberToken]) {
      return false
    }

    return obj.asInstanceOf[NumberToken].string == string
  }
}