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
                                     val variable: Variable) extends Split[DataType] {
  require(variable.continuous, "Cannot construct numeric split from factor !")

  override def goesLeft(d: DataType): Boolean = d(variable) < split
}

class FactorSplit[DataType <: Data](val leftIndices: Set[Int],
                                    val variable: Variable) extends Split[DataType] {
  require(!variable.continuous, "Cannot construct discrete split from numeric variable !")

  override def goesLeft(d: DataType): Boolean = leftIndices.contains(d(variable).toInt)
}