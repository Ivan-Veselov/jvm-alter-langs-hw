package ru.spbau.bachelors2015.veselov.evaluation

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
    private val prodEvaluator = new BinOpEvaluator(Map(TokenType.MulOp -> (_ * _),
                                                       TokenType.DivOp -> (_ / _)),
                                                   PrimaryEvaluator)

    private val sumEvaluator = new BinOpEvaluator(Map(TokenType.AddOp -> (_ + _),
                                                      TokenType.SubOp -> (_ - _)),
                                                  prodEvaluator)

    def eval(): Double = sumEvaluator.eval(new TokenViewer(tokens))

    // TODO: add parentheses
    private object PrimaryEvaluator extends Evaluator {
      override def eval(viewer: TokenViewer): Double = {
        if (viewer.currentToken().tokenType == TokenType.Number) {
          val result = viewer.currentToken().chars.toDouble
          viewer.move()

          return result
        }

        throw new InvalidExpressionException
      }
    }
  }
}
