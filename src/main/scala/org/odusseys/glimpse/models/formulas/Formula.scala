package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.DataFrame

/**
 * Created by umizrahi on 08/03/2016.
 */
trait Formula {
  def decode[T](data: DataFrame[T]): FormulaReader[T]
}

trait FormulaReader[T] {
  def responses: Array[T => Int]

  def variables: Array[T => Int]

}

class ColumnNamesFormula(val wildcardResponses: Boolean,
                         val wildCardVariables: Boolean,
                         val responses: Array[String],
                         val variables: Array[String]) extends Formula {
  override def decode[T](data: DataFrame[T]): FormulaReader[T] = new ColumnNamesFormulaReader(this, data)
}

class ColumnNamesFormulaReader[T](formula: ColumnNamesFormula, data: DataFrame[T]) extends FormulaReader[T] {
  override def responses: Array[(T) => Int] = ???

  override def variables: Array[(T) => Int] = ???
}

object Formula {

  val columnSeparator = "+"
  val formulaSeparator = "~"
  val wildcard = "."

  private def decompose(member: String) = {
    if (member.equals(wildcard)) {
      (true, Array[String]())
    } else {
      (false, member.split(columnSeparator).map(_.trim))
    }
  }

  def apply(s: String): Formula = {
    val split = s.split(formulaSeparator).map(_.trim)
    val (wildcardResponses, responses) = decompose(split(0))
    val (wildcardVariables, variables) = decompose(split(1))
    new ColumnNamesFormula(wildcardResponses, wildcardVariables, responses, variables)
  }
}
