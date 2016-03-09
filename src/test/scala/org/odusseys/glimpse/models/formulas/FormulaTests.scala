package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.io.Import
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 09/03/2016.
 */
class FormulaTests extends FunSuite {
  val data = Import.fromCSV("src/test/resources/data.csv")

  test("formula from names with multiple variables and no wildcards") {
    val form = Formula.fromNames("a ~ b + c").asInstanceOf[ColumnNamesFormula]
    assert(!form.wildcardResponses)
    assert(!form.wildCardVariables)
    assert(form.responses.toSet.equals(Set("a")))
    assert(form.variables.toSet.equals(Set("b", "c")))
    val reader = form.decodeFor(data)
    val mapping = data.mapping
    reader.responseIndices.map(i => mapping(i).name).zip(form.responses).foreach { case (u, v) => assert(u.equals(v)) }
    reader.variableIndices.map(i => mapping(i).name).zip(form.variables).foreach { case (u, v) => assert(u.equals(v)) }
    data.foreach { l =>
      reader.responses.map(r => r(l)).zip(reader.responseIndices.map(i => l(i))).foreach { case (u, v) => u == v }
      reader.variables.map(r => r(l)).zip(reader.variableIndices.map(i => l(i))).foreach { case (u, v) => u == v }
    }
  }

  test("formula from names with multiple variables and wildcard response") {
    val form = Formula.fromNames(". ~ b + c").asInstanceOf[ColumnNamesFormula]
    assert(form.wildcardResponses)
    assert(!form.wildCardVariables)
    assert(form.responses.toSet.equals(Set()))
    assert(form.variables.toSet.equals(Set("b", "c")))
    val reader = form.decodeFor(data)
    val mapping = data.mapping
    reader.responseIndices.map(i => mapping(i).name).zip(form.responses).foreach { case (u, v) => assert(u.equals(v)) }
    reader.variableIndices.map(i => mapping(i).name).zip(form.variables).foreach { case (u, v) => assert(u.equals(v)) }
    data.foreach { l =>
      reader.responses.map(r => r(l)).zip(reader.responseIndices.map(i => l(i))).foreach { case (u, v) => u == v }
      reader.variables.map(r => r(l)).zip(reader.variableIndices.map(i => l(i))).foreach { case (u, v) => u == v }
    }
  }

  test("formula from names with wildcard variables and one response") {
    val form = Formula.fromNames("a ~ .").asInstanceOf[ColumnNamesFormula]
    assert(!form.wildcardResponses)
    assert(form.wildCardVariables)
    assert(form.responses.toSet.equals(Set("a")))
    assert(form.variables.toSet.equals(Set()))
    val reader = form.decodeFor(data)
    val mapping = data.mapping
    reader.responseIndices.map(i => mapping(i).name).zip(form.responses).foreach { case (u, v) => assert(u.equals(v)) }
    reader.variableIndices.map(i => mapping(i).name).zip(form.variables).foreach { case (u, v) => assert(u.equals(v)) }
    data.foreach { l =>
      reader.responses.map(r => r(l)).zip(reader.responseIndices.map(i => l(i))).foreach { case (u, v) => u == v }
      reader.variables.map(r => r(l)).zip(reader.variableIndices.map(i => l(i))).foreach { case (u, v) => u == v }
    }
  }

  test("implicit conversion should yield the same formula (with column names)") {
    val form: Formula = "a ~ b + c"
    val f = Formula.fromNames("a ~ b + c").asInstanceOf[ColumnNamesFormula]
    val f2 = form.asInstanceOf[ColumnNamesFormula]
    assert(f.wildcardResponses == f2.wildcardResponses)
    assert(f.wildCardVariables == f2.wildCardVariables)
    assert(f.responses.toSet.equals(f2.responses.toSet))
    assert(f.variables.toSet.equals(f2.variables.toSet))
  }

}
