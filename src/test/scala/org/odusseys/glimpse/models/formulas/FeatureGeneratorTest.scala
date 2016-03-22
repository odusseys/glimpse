package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.io.Import
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 22/03/2016.
 */
class FeatureGeneratorTest extends FunSuite {

  val data = Import.fromCSV("src/test/resources/data.csv")


  /*From column names */

  test("multiple variables") {
    val form = new NewFormula("a = b,c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 1)
    assert(gen.factorVariables.length == 1)
  }

  test("multiple responses") {
    val form = new NewFormula("a,b = c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 2)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard response") {
    val form = new NewFormula("* = c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 2)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard response with exception") {
    val form = new NewFormula("* - a = c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard response with multiple exceptions") {
    val form = new NewFormula("* - a,b = c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 0)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard variable") {
    val form = new NewFormula("a = *", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 1)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard variable with exception") {
    val form = new NewFormula("a = * - b", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
  }

  test("wildcard variable with multiple exceptions") {
    val form = new NewFormula("a = * - b,c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 0)
  }

  test("variable plus sinus of that variable") {
    val form = new NewFormula("a, sin(a) = b,c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 2)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 1)
    assert(gen.factorVariables.length == 1)
    val a = gen.numericResponses(0)
    val sinA = gen.numericResponses(1)
    data.foreach { l => assert(math.abs(math.sin(a(l)) - sinA(l)) < 0.001) }
  }

  test("response plus sinus of that response") {
    val form = new NewFormula("b,c = a, sin(a)", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 1)
    assert(gen.factorResponses.length == 1)
    assert(gen.numericVariables.length == 2)
    assert(gen.factorVariables.length == 0)
    val a = gen.numericVariables(0)
    val sinA = gen.numericVariables(1)
    data.foreach { l => assert(math.abs(math.sin(a(l)) - sinA(l)) < 0.001) }
  }

  test("variables plus sum of these variable") {
    val form = new NewFormula("c = a, b, a + b", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 0)
    assert(gen.factorResponses.length == 1)
    assert(gen.numericVariables.length == 3)
    assert(gen.factorVariables.length == 0)
    val a = gen.numericVariables(0)
    val b = gen.numericVariables(1)
    val sum = gen.numericVariables(2)
    data.foreach { l => assert(math.abs(a(l) + b(l) - sum(l)) < 0.001) }
  }

  test("responses plus sum of these responses") {
    val form = new NewFormula("a, b, a + b = c", true)
    val gen = form.decodeFor(data)
    assert(gen.numericResponses.length == 3)
    assert(gen.factorResponses.length == 0)
    assert(gen.numericVariables.length == 0)
    assert(gen.factorVariables.length == 1)
    val a = gen.numericResponses(0)
    val b = gen.numericResponses(1)
    val sum = gen.numericResponses(2)
    data.foreach { l => assert(math.abs(a(l) + b(l) - sum(l)) < 0.001) }
  }

}
