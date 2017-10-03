package ru.spbau.bachelors2015.veselov

import ru.spbau.bachelors2015.veselov.evaluation.{EmptyExpressionException, ExpressionEvaluator, InvalidExpressionException}
import ru.spbau.bachelors2015.veselov.tokenization.InvalidTokenException

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    Console.print("Enter arithmetical expression:\n")

    try {
      Console.print(ExpressionEvaluator.eval(StdIn.readLine()))
    } catch {
      case _: EmptyExpressionException =>
        Console.print("Empty expression is not allowed")
      case _ : InvalidTokenException | _ : InvalidExpressionException =>
        Console.print("Invalid expression entered")
    }
  }
}
