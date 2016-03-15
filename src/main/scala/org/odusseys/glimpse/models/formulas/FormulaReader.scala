package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{DataFrame, Data}

/**
 * Created by umizrahi on 14/03/2016.
 */
sealed abstract class FormulaReader[T <: Data](data: DataFrame[T]) {
  def numericResponses: Array[T => Double] = numericResponseIndices.map(i => (t: T) => t(i))

  def numericVariables: Array[T => Double] = numericVariableIndices.map(i => (t: T) => t(i))

  def numericResponseIndices: Array[Int]

  def numericVariableIndices: Array[Int]

  def factorResponses: Array[T => Int] = factorResponseIndices.map(i => (t: T) => t(i).toInt)

  def factorVariables: Array[T => Int] = factorVariableIndices.map(i => (t: T) => t(i).toInt)

  def factorResponseIndices: Array[Int]

  def factorVariableIndices: Array[Int]

  def signature = (numericResponseIndices.length, numericVariableIndices.length)

  //these are lazy so that they don't call response/variableIndices methods during init
  private lazy val responseIndexMap = numericResponseIndices.indices.map(i => (numericResponseIndices(i), i)).toMap
  private lazy val variableIndexMap = numericVariableIndices.indices.map(i => (numericVariableIndices(i), i)).toMap

  def sparseNumericResponses: T => Iterator[(Int, Double)] =
    (d: T) => d.indices.withFilter(responseIndexMap.contains)
      .map(i => (responseIndexMap(i), d(i)))

  def sparseNumericVariables: T => Iterator[(Int, Double)] =
    (d: T) => d.indices.withFilter(variableIndexMap.contains)
      .map(i => (variableIndexMap(i), d(i)))

  def sparseFactorResponses: T => Iterator[(Int, Int)] =
    (d: T) => d.indices.withFilter(responseIndexMap.contains)
      .map(i => (responseIndexMap(i), d(i).toInt))

  def sparseFactorVariables: T => Iterator[(Int, Int)] =
    (d: T) => d.indices.withFilter(variableIndexMap.contains)
      .map(i => (variableIndexMap(i), d(i).toInt))

  def numericVariableNames = {
    numericVariableIndices.map(i => data.mapping(i).name)
  }

  def factorVariableNames = {
    factorVariableIndices.map(i => data.mapping(i).name)
  }

  def numericResponseNames = {
    numericResponseIndices.map(i => data.mapping(i).name)
  }

  def factorResponseNames = {
    factorResponseIndices.map(i => data.mapping(i).name)
  }

}

class ColumnNamesFormulaReader[T <: Data](formula: ColumnNamesFormula,
                                          data: DataFrame[T]) extends FormulaReader[T](data) {

  val mapping = data.mapping.collect { case (i, v) => (v.name, i) }
  val unknown = (formula.responses ++ formula.variables).filter(!mapping.contains(_))
  require(unknown.isEmpty, s"Unknown column names : ${unknown.mkString(", ")}")

  val (_numericResponseIndices, _numericVariableIndices) = {
    val mapper = data.mapping.collect { case (i, v) if v.continuous => (v.name, i) }
    if (formula.wildcardResponses) {
      val v = formula.variables.flatMap(mapper.get)
      ((mapper.values.toSet -- v).toArray, v)
    } else if (formula.wildCardVariables) {
      val r = formula.responses.flatMap(mapper.get)
      (r, (mapper.values.toSet -- r).toArray)
    } else {
      (formula.responses.flatMap(mapper.get), formula.variables.flatMap(mapper.get))
    }
  }

  val (_factorResponseIndices, _factorVariableIndices) = {
    val mapper = data.mapping.collect { case (i, v) if !v.continuous => (v.name, i) }
    if (formula.wildcardResponses) {
      val v = formula.variables.flatMap(mapper.get)
      ((mapper.values.toSet -- v).toArray, v)
    } else if (formula.wildCardVariables) {
      val r = formula.responses.flatMap(mapper.get)
      (r, (mapper.values.toSet -- r).toArray)
    } else {
      (formula.responses.flatMap(mapper.get), formula.variables.flatMap(mapper.get))
    }
  }

  override def numericResponseIndices: Array[Int] = _numericResponseIndices

  override def numericVariableIndices: Array[Int] = _numericVariableIndices

  override def factorResponseIndices: Array[Int] = _factorResponseIndices

  override def factorVariableIndices: Array[Int] = _factorVariableIndices
}

class ColumnIndexFormulaReader[T <: Data](formula: ColumnIndexFormula, data: DataFrame[T]) extends FormulaReader[T](data) {

  val allIndices = data.mapping.toMap.keySet
  val outOfBounds = formula.responses ++ formula.variables filter (_ < allIndices.size)
  require(outOfBounds.isEmpty, s"Indices out of bounds : ${outOfBounds.mkString(", ")}")

  val (_numericResponseIndices, _numericVariableIndices) = {
    val indices = data.mapping.filter(_._2.continuous).keySet
    if (formula.wildcardResponses) {
      ((indices -- formula.variables).toArray, formula.variables.filter(indices.contains))
    } else if (formula.wildCardVariables) {
      (formula.responses.filter(indices.contains), (indices -- formula.responses).toArray)
    } else {
      (formula.responses.filter(indices.contains), formula.variables.filter(indices.contains))
    }
  }

  val (_factorResponseIndices, _factorVariableIndices) = {
    val indices = data.mapping.filter(!_._2.continuous).keySet
    if (formula.wildcardResponses) {
      ((indices -- formula.variables).toArray, formula.variables.filter(indices.contains))
    } else if (formula.wildCardVariables) {
      (formula.responses.filter(indices.contains), (indices -- formula.responses).toArray)
    } else {
      (formula.responses.filter(indices.contains), formula.variables.filter(indices.contains))
    }
  }

  override def numericResponseIndices: Array[Int] = _numericResponseIndices

  override def numericVariableIndices: Array[Int] = _numericVariableIndices

  override def factorResponseIndices: Array[Int] = _factorResponseIndices

  override def factorVariableIndices: Array[Int] = _factorVariableIndices
}
