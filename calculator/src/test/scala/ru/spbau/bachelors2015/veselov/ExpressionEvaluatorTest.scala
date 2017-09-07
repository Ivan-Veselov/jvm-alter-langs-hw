package ru.spbau.bachelors2015.veselov

import org.scalatest.FunSuite

class ExpressionEvaluatorTest extends FunSuite {
  test("test addition") {
    assert(ExpressionEvaluator.eval("1 + 2 + 3 + 4 + 5") == 1d + 2d + 3d + 4d + 5d)
  }

  test("test multiplication") {
    assert(ExpressionEvaluator.eval("1 * 2 * 3 * 4 * 5") == 1d * 2d * 3d * 4d * 5d)
  }

  test("test addition and multiplication") {
    assert(ExpressionEvaluator.eval("1 + 2 * 3 + 4 * 5") == 1d + 2d * 3d + 4d * 5d)
  }
}
