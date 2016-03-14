package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.data.ColumnMapping._

/**
 * Created by umizrahi on 08/03/2016.
 */
sealed trait Formula {
  def decodeFor[T <: Data](data: DataFrame[T]): FormulaReader[T]
}

class ColumnNamesFormula(val wildcardResponses: Boolean,
                         val wildCardVariables: Boolean,
                         val responses: Array[String],
                         val variables: Array[String]) extends Formula {
  override def decodeFor[T <: Data](data: DataFrame[T]): FormulaReader[T] =
    new ColumnNamesFormulaReader(this, data)
}

class ColumnIndexFormula(val wildcardResponses: Boolean,
                         val wildCardVariables: Boolean,
                         val responses: Array[Int],
                         val variables: Array[Int]) extends Formula {
  override def decodeFor[T <: Data](data: DataFrame[T]): FormulaReader[T] = new ColumnIndexFormulaReader(this, data)
}

object Formula {

  val columnSeparator = "\\+"
  val formulaSeparator = "~"
  val wildcard = "."

  private def decompose(member: String) = {
    val trimmed = member.trim
    if (trimmed.length == 0) {
      (false, Array[String]())
    } else if (trimmed.equals(wildcard)) {
      (true, Array[String]())
    } else {
      (false, trimmed.split(columnSeparator).map(_.trim))
    }
  }

  def fromNames(s: String): Formula = {
    val split = s.split(formulaSeparator).map(_.trim)
    val (wildcardResponses, responses) = decompose(split(0))
    val (wildcardVariables, variables) = decompose(split(1))
    require(!(wildcardResponses && wildcardVariables), "Cannot have both wildcard responses and variables")
    new ColumnNamesFormula(wildcardResponses, wildcardVariables, responses, variables)
  }

  def fromIndices(s: String): Formula = {
    val split = s.split(formulaSeparator).map(_.trim)
    val (wildcardResponses, responses) = decompose(split(0))
    val (wildcardVariables, variables) = decompose(split(1))
    require(!(wildcardResponses && wildcardVariables), "Cannot have both wildcard responses and variables")
    new ColumnIndexFormula(wildcardResponses, wildcardVariables, responses.map(_.toInt), variables.map(_.toInt))
  }

  implicit def toFormula(s: String): Formula = fromNames(s)

}
