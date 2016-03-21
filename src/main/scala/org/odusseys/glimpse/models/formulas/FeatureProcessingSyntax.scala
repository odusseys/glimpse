package org.odusseys.glimpse.models.formulas

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by umizrahi on 21/03/2016.
 */

sealed abstract class FeatureProcessingSyntax

case class Raw(s: String) extends FeatureProcessingSyntax

case class Unary(processor: String, argument: FeatureProcessingSyntax) extends FeatureProcessingSyntax

case class Binary(processor: String, first: FeatureProcessingSyntax, second: FeatureProcessingSyntax) extends FeatureProcessingSyntax


object FeatureProcessingSyntax {

  def tokenize(s: String) = {
    val parensDone = s.replaceAll("\\(", " \\( ").replaceAll("\\)", " \\) ")
    val functionsDone = FormulaFunction.names.foldLeft(parensDone)((u, v) => u.replace(v, s" $v "))
    val expanded = FormulaOperators.names.foldLeft(functionsDone)((u, v) => u.replace(v, s" $v "))
    expanded.split(" +")
  }

  def shunt(s: Array[String]) = {
    val operatorStack = new mutable.Stack[String]
    val output = new ArrayBuffer[String]

    s.foreach { token =>
      if (token.equals("(")) {
        operatorStack.push(token)
      } else if (token.equals(")")) {
        while (operatorStack.nonEmpty && !operatorStack.top.equals("(")) {
          output.append(operatorStack.pop())
        }
        if (operatorStack.isEmpty) {
          throw new IllegalArgumentException("Parentheses do not match !")
        } else {
          operatorStack.pop()
          if (operatorStack.nonEmpty && FormulaFunction.isFunction(operatorStack.top)) {
            output.append(operatorStack.pop())
          }
        }
      } else if (FormulaFunction.isFunction(token)) {
        operatorStack.push(token)
      } else if (FormulaOperators.isOperator(token)) {
        val prec = FormulaOperators.precedence(token)
        while (operatorStack.nonEmpty
          && FormulaOperators.isOperator(operatorStack.top)
          && FormulaOperators.precedence(operatorStack.top) >= prec) {
          output.append(operatorStack.pop())
        }
        operatorStack.push(token)
      } else {
        output.append(token)
      }
    }
    operatorStack.foreach { op =>
      if (op.equals("(") || op.equals(")")) throw new IllegalArgumentException("Parentheses do not match !")
      output.append(op)
    }
    output.toList
  }

  def rpnToAst(tokens: List[String]) = {
    val expressions = new mutable.Stack[FeatureProcessingSyntax]
    tokens.foreach { token =>
      if (FormulaFunction.isFunction(token)) {
        require(expressions.nonEmpty, "No arguments found to apply to function " + token)
        val arg = expressions.pop()
        expressions.push(Unary(token, arg))
      } else if (FormulaOperators.isOperator(token)) {
        require(expressions.nonEmpty, "No arguments found to apply to function " + token)
        val second = expressions.pop()
        require(expressions.nonEmpty, "Missing argument for operator " + token)
        val first = expressions.pop()
        expressions.push(Binary(token, first, second))
      } else {
        expressions.push(Raw(token))
      }
    }
    expressions.foreach(println)
    require(expressions.size == 1, "Dangling variables in expression")
    expressions.head
  }

  def parseSyntaxTree(s: String) = rpnToAst(shunt(tokenize(s)))

  def main(args: Array[String]) {
    val u = "a + b / c - sin(ahaha) + (cos(t) + a)"
    parseSyntaxTree(u)
  }

}

