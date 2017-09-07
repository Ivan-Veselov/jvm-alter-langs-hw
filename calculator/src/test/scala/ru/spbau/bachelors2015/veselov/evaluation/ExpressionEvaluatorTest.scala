package ru.spbau.bachelors2015.veselov.evaluation

import org.scalatest.FunSuite

// TODO: test invalid expressions
class ExpressionEvaluatorTest extends FunSuite {
  test("addition") {
    assert(ExpressionEvaluator.eval("1 + 2 + 3 + 4 + 5") == 1d + 2d + 3d + 4d + 5d)
  }

  test("subtraction") {
    assert(ExpressionEvaluator.eval("1 - 2 - 3 - 4 - 5") == 1d - 2d - 3d - 4d - 5d)
  }

  test("addition and subtraction") {
    assert(ExpressionEvaluator.eval("1 + 2 - 3 + 4 - 5") == 1d + 2d - 3d + 4d - 5d)
  }

  test("multiplication") {
    assert(ExpressionEvaluator.eval("1 * 2 * 3 * 4 * 5") == 1d * 2d * 3d * 4d * 5d)
  }

  test("division") {
    assert(ExpressionEvaluator.eval("1 / 2 / 3 / 4 / 5") == 1d / 2d / 3d / 4d / 5d)
  }

  test("multiplication and division") {
    assert(ExpressionEvaluator.eval("1 * 2 / 3 * 4 / 5") == 1d * 2d / 3d * 4d / 5d)
  }

  test("all arithmetical operations in one expression") {
    assert(ExpressionEvaluator.eval("1 + 2 / 3 * 4 - 5 * 6") == 1d + 2d / 3d * 4d - 5d * 6d)
  }

  test("simple parentheses") {
    assert(ExpressionEvaluator.eval("1 - (2 + 3)") == 1d - (2d + 3d))
  }

  test("multiplication of parentheses") {
    assert(ExpressionEvaluator.eval("(1 + 2) * (3 + 4) * (5 + 6)") ==
                                    (1d + 2d) * (3d + 4d) * (5d + 6d))
  }

  test("big level of parentheses") {
    assert(ExpressionEvaluator.eval("1 - (2 - (3 - (4 - (5 - (6 - 7)))))") ==
                                    1d - (2d - (3d - (4d - (5d - (6d - 7d))))))
  }

  test("mix of all operations") {
    assert(ExpressionEvaluator.eval("(1 + 2) * 3 - (4 - 5 * (6 + 7)) / 8") ==
                                    (1d + 2d) * 3d - (4d - 5d * (6d + 7d)) / 8d)
  }
}
