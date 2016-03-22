package org.odusseys.glimpse.models.formulas

import org.scalatest.FunSuite

/**
 * Created by umizrahi on 22/03/2016.
 */
class FeatureProcessingSyntaxTest extends FunSuite {

  import FeatureProcessingSyntax._

  test("Tokenize expression with various white space, operators, functions and parentheses") {
    val exp1 = "a + b - sin(A+b)   * exp(A + log(c))"
    val expected = List("a", "+", "b", "-", "sin", "(", "A", "+", "b", ")", "*", "exp", "(", "A", "+", "log", "(", "c", ")", ")")
    tokenize(exp1).zip(expected).foreach { case (u, v) => assert(u.equals(v)) }
  }

  test("Shunting should give expected RPN notation") {
    val expression = "a + (b * c + a) + sin(x)"
    val tokens = tokenize(expression)
    val rpn = Array("a", "b", "c", "*", "a", "+", "+", "x", "sin", "+")
    shunt(tokens).zip(rpn).foreach { case (u, v) => assert(u.equals(v)) }
  }

  test("Tokenized binary expression should match syntax tree") {
    val expression = "a + b"
    val tree = parseSyntaxTree(expression)
    val expected = Binary("+", Raw("a"), Raw("b"))
    assert(tree == expected)
  }

  test("Tokenized function expression should match syntax tree") {
    val expression = "sin(a)"
    val tree = parseSyntaxTree(expression)
    val expected = Unary("sin", Raw("a"))
    assert(tree == expected)
  }

  test("Tokenized variable should match syntax tree") {
    val expression = "a"
    val tree = parseSyntaxTree(expression)
    val expected = Raw("a")
    assert(tree == expected)
  }

  test("Complex expression should match syntax tree") {
    val expression = "sin(a + exp(b)) - d"
    val tree = parseSyntaxTree(expression)
    val expected = Binary("-", Unary("sin", Binary("+", Raw("a"), Unary("exp", Raw("b")))), Raw("d"))
    assert(tree == expected)
  }

}
