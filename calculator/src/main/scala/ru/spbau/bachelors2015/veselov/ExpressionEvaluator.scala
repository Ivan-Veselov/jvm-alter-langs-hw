package ru.spbau.bachelors2015.veselov

import ru.spbau.bachelors2015.veselov.tokenization.{ExpressionTokenizer, Token, TokenType}

// TODO: add docs
// TODO: add docs to exception
object ExpressionEvaluator {
  def eval(chars: CharSequence): Double = {
    new InnerState(ExpressionTokenizer.tokenList(chars)
                                      .filter(_.tokenType != TokenType.Whitespace)
                  ).eval()
  }

  private class InnerState(var tokens: List[Token]) {
    def eval(): Double = evalSum()

    // TODO: add subtraction
    private def evalSum(): Double = {
      var res = evalProd()
      while (tokens.nonEmpty && tokens.head.tokenType == TokenType.AddOp) {
        tokens = tokens.drop(1)
        res += evalProd()
      }

      return res
    }

    // TODO: add division
    private def evalProd(): Double = {
      var res = evalPrimary()
      while (tokens.nonEmpty && tokens.head.tokenType == TokenType.MulOp) {
        tokens = tokens.drop(1)
        res *= evalPrimary()
      }

      return res
    }

    // TODO: add parentheses
    private def evalPrimary(): Double = {
      if (tokens.head.tokenType == TokenType.Number) {
        val result = tokens.head.chars.toDouble
        tokens = tokens.drop(1)
        return result
      }

      throw new InvalidExpressionException
    }
  }
}
