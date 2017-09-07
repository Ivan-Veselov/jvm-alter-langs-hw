package ru.spbau.bachelors2015.veselov.tokenization

/**
  * An enumeration of possible arithmetical tokens.
  */
object TokenType extends Enumeration {
  type TokenType = Value
  val Number, AddOp, SubOp, MulOp, DivOp, LeftParen, RightParen, Whitespace = Value

  def regex(tokenType: TokenType): String = tokenType match {
    case Number => raw"(0|([1-9]\d*))(\.\d+)?"
    case AddOp => raw"\+"
    case SubOp => raw"-"
    case MulOp => raw"\*"
    case DivOp => raw"/"
    case LeftParen => raw"\("
    case RightParen => raw"\)"
    case Whitespace => raw"\s+"
  }
}
