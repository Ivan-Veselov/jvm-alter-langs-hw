package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.TokenType.TokenType
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

    private def evalSum(): Double = {
      val operations = Map[TokenType, (Double, Double) => Double](TokenType.AddOp -> (_ + _),
                                                                  TokenType.SubOp -> (_ - _))

      var res = evalProd()

      while (tokens.nonEmpty && operations.contains(tokens.head.tokenType)) {
        val op = operations.apply(tokens.head.tokenType)
        tokens = tokens.drop(1)

        res = op(res, evalProd())
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
