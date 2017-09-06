package ru.spbau.bachelors2015.veselov

import ru.spbau.bachelors2015.veselov.tokenization.ExpressionTokenizer

class ExpressionEvaluator(tokenizer: ExpressionTokenizer) {
  private var evaluated = false

  private var value: Double = 0.0

  def eval(): Double = {
    if (evaluated) {
      return value
    }

    value = evalSum()
    evaluated = true

    return value
  }

  private def evalSum(): Double = {
    var res = evalProd()
    return res
  }

  private def evalProd(): Double = {
    return evalPrimary()
  }

  private def evalPrimary(): Double = {
    return 0.0
  }
}
