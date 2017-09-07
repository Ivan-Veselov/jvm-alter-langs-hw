package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.{ExpressionTokenizer, Token, TokenType}

/**
  * Singleton which can evaluate a given string to Double value.
  */
object ExpressionEvaluator {
  /**
    * Evaluates a given string with arithmetical expression to its value.
    *
    * @param chars a string which represents arithmetical expression.
    * @return evaluated result.
    */
  def eval(chars: CharSequence): Double = {
    new InnerState(ExpressionTokenizer.tokenList(chars)
                                      .filter(_.tokenType != TokenType.Whitespace)
                  ).eval()
  }

  private class InnerState(tokens: List[Token]) {
    private val prodEvaluator = new BinOpEvaluator(Map(TokenType.MulOp -> (_ * _),
                                                       TokenType.DivOp -> (_ / _)),
                                                   PrimaryEvaluator)

    private val sumEvaluator = new BinOpEvaluator(Map(TokenType.AddOp -> (_ + _),
                                                      TokenType.SubOp -> (_ - _)),
                                                  prodEvaluator)

    def eval(): Double = sumEvaluator.eval(new TokenViewer(tokens))

    private object PrimaryEvaluator extends Evaluator {
      override def eval(viewer: TokenViewer): Double = {
        if (!viewer.nonEmpty()) {
          throw new InvalidExpressionException
        }

        var result = 0d

        viewer.currentToken().tokenType match {
          case TokenType.Number =>
            result = viewer.currentToken().chars.toDouble
            viewer.move()

          case TokenType.LeftParen =>
            viewer.move()
            result = sumEvaluator.eval(viewer)

            if (!viewer.nonEmpty() || viewer.currentToken().tokenType != TokenType.RightParen) {
              throw new InvalidExpressionException
            }

            viewer.move()

          case _ => throw new InvalidExpressionException
        }

        return result
      }
    }
  }
}
