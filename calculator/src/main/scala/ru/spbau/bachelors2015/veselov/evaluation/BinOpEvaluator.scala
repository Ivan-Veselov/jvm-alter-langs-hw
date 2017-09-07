package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.TokenType
import ru.spbau.bachelors2015.veselov.tokenization.TokenType.TokenType

class BinOpEvaluator(operations: Map[TokenType, (Double, Double) => Double],
                     nextLevel: Evaluator) extends Evaluator {
  override def eval(viewer: TokenViewer): Double = {
    /*val operations = Map[TokenType, (Double, Double) => Double](TokenType.AddOp -> (_ + _),
      TokenType.SubOp -> (_ - _))*/

    var res = nextLevel.eval(viewer)

    while (viewer.nonEmpty && operations.contains(viewer.currentToken().tokenType)) {
      val op = operations.apply(viewer.currentToken().tokenType)
      viewer.move()

      res = op(res, nextLevel.eval(viewer))
    }

    return res
  }
}
