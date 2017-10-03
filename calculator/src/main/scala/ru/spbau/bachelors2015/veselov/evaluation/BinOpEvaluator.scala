package ru.spbau.bachelors2015.veselov.evaluation

import ru.spbau.bachelors2015.veselov.tokenization.TokenType.TokenType

/**
  * Can evaluate a prefix of given sequence of tokens. This evaluator is intended to call other
  * evaluators recursively to handle constructions of next levels. Current level is all operations
  * of given type which aren't surrounded by parentheses and which starting from the beginning of
  * a given TokenViewer form a valid expression.
  *
  * @param operations a current level operations.
  * @param nextLevel an Evaluator to evaluate expressions of next level.
  */
class BinOpEvaluator(operations: Map[TokenType, (Double, Double) => Double],
                     nextLevel: Evaluator) extends Evaluator {
  /**
    * Consumes and evaluates a prefix of given TokenViewer as described in class documentation.
    *
    * @param viewer a sequence of tokens represented by a TokenViewer.
    * @return evaluated result.
    */
  override def eval(viewer: TokenViewer): Double = {
    var res = nextLevel.eval(viewer)

    while (viewer.nonEmpty && operations.contains(viewer.currentToken().tokenType)) {
      val op = operations.apply(viewer.currentToken().tokenType)
      viewer.move()

      res = op(res, nextLevel.eval(viewer))
    }

    return res
  }
}
