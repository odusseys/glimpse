package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{Data, FactorColumn}
import org.odusseys.glimpse.models.formulas.{FactorFeature, NumericFeature}

import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

/**
  * Describes s (continuous or discrete) split in a decision tree
  * Created by umizrahi on 14/03/2016.
  */
sealed trait Split[DataType <: Data] {
  def goesLeft(d: DataType): Boolean

  def toJSON: String
}

class NumericSplit[DataType <: Data](val split: Double,
                                     val variable: NumericFeature) extends Split[DataType] {
  override def goesLeft(d: DataType): Boolean = variable(d) <= split

  override def toJSON: String = pretty(render(
    ("variable" -> variable.name) ~ ("split" -> split)
  ))
}

class FactorSplit[DataType <: Data](val leftIndices: Set[Int],
                                    val levelMapping: Int => String,
                                    val variable: FactorFeature) extends Split[DataType] {
  override def goesLeft(d: DataType): Boolean = leftIndices.contains(variable(d))

  override def toJSON: String = pretty(render(
    ("variable" -> variable.name) ~ ("leftLevels" -> leftIndices.map(levelMapping).toList)
  ))

}