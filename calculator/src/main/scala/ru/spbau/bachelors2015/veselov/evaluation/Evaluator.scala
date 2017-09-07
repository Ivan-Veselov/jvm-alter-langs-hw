package ru.spbau.bachelors2015.veselov.evaluation

trait Evaluator {
  def eval(viewer: TokenViewer): Double
}
