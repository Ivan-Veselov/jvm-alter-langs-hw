package ru.spbau.bachelors2015.veselov.evaluation

/**
  * Evaluator can consume a prefix of TokenViewer sequence and transform it into a resulting value.
  */
trait Evaluator {
  /**
    * Evaluates given sequence of tokens by consuming valid prefix of this sequence and converting
    * it into a Double value.
    *
    * @param viewer a sequence of tokens represented by a TokenViewer.
    * @return evaluated result.
    */
  def eval(viewer: TokenViewer): Double
}
