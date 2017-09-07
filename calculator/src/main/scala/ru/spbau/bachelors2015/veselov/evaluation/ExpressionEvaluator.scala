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

  private class InnerState(tokens: List[Token]) {
    private val viewer = new TokenViewer(tokens)

    def eval(): Double = evalSum()

    private def evalSum(): Double = {
      val operations = Map[TokenType, (Double, Double) => Double](TokenType.AddOp -> (_ + _),
                                                                  TokenType.SubOp -> (_ - _))

      var res = evalProd()

      while (viewer.nonEmpty && operations.contains(viewer.currentToken().tokenType)) {
        val op = operations.apply(viewer.currentToken().tokenType)
        viewer.move()

        res = op(res, evalProd())
      }

      return res
    }

    // TODO: add division
    private def evalProd(): Double = {
      var res = evalPrimary()
      while (viewer.nonEmpty && viewer.currentToken().tokenType == TokenType.MulOp) {
        viewer.move()
        res *= evalPrimary()
      }

      return res
    }

    // TODO: add parentheses
    private def evalPrimary(): Double = {
      if (viewer.currentToken().tokenType == TokenType.Number) {
        val result = viewer.currentToken().chars.toDouble
        viewer.move()
        return result
      }

      throw new InvalidExpressionException
    }
  }
}
