package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.data.ColumnMapping._

/**
 * Created by umizrahi on 08/03/2016.
 */
trait Formula {
  def decodeFor[T <: Data](data: DataFrame[T]): FormulaReader[T]
}

trait FormulaReader[T <: Data] {
  def responses: Array[T => Double] = responseIndices.map(i => (t: T) => t(i))

  def variables: Array[T => Double] = variableIndices.map(i => (t: T) => t(i))

  def responseIndices: Array[Int]

  def variableIndices: Array[Int]

  private val responseIndexMap = responseIndices.indices.map(i => (responseIndices(i), i)).toMap
  private val variableIndexMap = variableIndices.indices.map(i => (variableIndices(i), i)).toMap

  def sparseResponses: T => Iterator[(Int, Double)] = (d: T) => d.indices.withFilter(responseIndexMap.contains).map(i => (responseIndexMap(i), d(i)))

  def sparseVariables: T => Iterator[(Int, Double)] = (d: T) => d.indices.withFilter(variableIndexMap.contains).map(i => (variableIndexMap(i), d(i)))

}

class ColumnNamesFormula(val wildcardResponses: Boolean,
                         val wildCardVariables: Boolean,
                         val responses: Array[String],
                         val variables: Array[String]) extends Formula {
  override def decodeFor[T](data: DataFrame[T]): FormulaReader[T] = new ColumnNamesFormulaReader(this, data)
}

class ColumnNamesFormulaReader[T <: Data](formula: ColumnNamesFormula, data: DataFrame[T]) extends FormulaReader[T] {

  val (_responseIndices, _variableIndices) = {
    val mapper = data.getMapping.map { case (i, v) => (v.name, i) }
    val unknown = (formula.responses ++ formula.variables).filter(!mapper.contains(_))
    require(unknown.isEmpty, s"Unknown column names : ${unknown.mkString(", ")}")
    if (formula.wildcardResponses) {
      val v = formula.variables.map(mapper.apply)
      ((mapper.values.toSet -- v).toArray, v)
    } else {
      val r = formula.responses.map(mapper.apply)
      (r, (mapper.values.toSet -- r).toArray)
    }
  }

  override def responseIndices: Array[Int] = _responseIndices

  override def variableIndices: Array[Int] = _variableIndices
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
    require(!(wildcardResponses && wildcardVariables), "Cannot have both wildcard responses and variables")
    new ColumnNamesFormula(wildcardResponses, wildcardVariables, responses, variables)
  }
}
