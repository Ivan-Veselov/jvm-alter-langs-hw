package ru.spbau.bachelors2015.veselov

import ru.spbau.bachelors2015.veselov.tokenization.ExpressionTokenizer
import ru.spbau.bachelors2015.veselov.tokenization.tokens.{AddOpToken, NumberToken, Token}

// TODO: add docs
// TODO: add docs to exception
// TODO: add end token
object ExpressionEvaluator {
  def eval(chars: CharSequence): Double = {
    new InnerState(ExpressionTokenizer.tokenList(chars)).eval()
  }

  private class InnerState(val tokens: List[Token]) {
    private val currentToken = new TokenPointer()

    def eval(): Double = evalSum()

    // TODO: add subtraction
    private def evalSum(): Double = {
      var res = evalProd()
      while (currentToken.token != null && currentToken.token.isInstanceOf[AddOpToken]) {
        currentToken.next()
        res += evalProd()
      }

      return res
    }

    // TODO: implement
    private def evalProd(): Double = {
      return evalPrimary()
    }

    // TODO: add parentheses
    private def evalPrimary(): Double = {
      if (currentToken.token.isInstanceOf[NumberToken]) {
        val result = currentToken.token.asInstanceOf[NumberToken].string.toDouble
        currentToken.next()
        return result
      }

      throw new InvalidExpressionException
    }

    private class TokenPointer {
      private val iterator = tokens.iterator

      var token: Token =
        if (!iterator.hasNext) {
          throw new InvalidExpressionException
        } else {
          iterator.next()
        }

      def next(): Unit = {
        if (iterator.isEmpty) {
          token = null
        } else {
          token = iterator.next()
        }
      }
    }
  }
}
