package ru.spbau.bachelors2015.veselov

import org.scalatest.FunSuite

class ExpressionEvaluatorTest extends FunSuite {
  test("test addition") {
    // TODO: check double values correctly
    assert(ExpressionEvaluator.eval("1+2+3+4+5") == 15)
  }
}
