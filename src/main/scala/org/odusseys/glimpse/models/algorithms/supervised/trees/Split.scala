package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{Variable, Data}

/**
 * Describes s (continuous or discrete) split in a decision tree
 * Created by umizrahi on 14/03/2016.
 */
sealed trait Split[DataType <: Data] {
  def goesLeft(d: DataType): Boolean
}

class NumericSplit[DataType <: Data](val split: Double,
                                     val variable: DataType => Double,
                                     val variableName: String) extends Split[DataType] {
  override def goesLeft(d: DataType): Boolean = variable(d) <= split
}

class FactorSplit[DataType <: Data](val leftIndices: Set[Int],
                                    val variable: DataType => Int,
                                    val variableName: String) extends Split[DataType] {
  override def goesLeft(d: DataType): Boolean = leftIndices.contains(variable(d))
}