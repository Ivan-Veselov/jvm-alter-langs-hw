package ru.spbau.bachelors2015.veselov.tokenization

class NumberToken(val value: Double) extends Token {
  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[NumberToken]) {
      return false
    }

    return obj.asInstanceOf[NumberToken].value == value
  }
}