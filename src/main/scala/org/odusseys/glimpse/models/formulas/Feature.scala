package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data._

/**
 * Created by umizrahi on 17/03/2016.
 */


abstract class Feature[T: Numeric] {
  def apply[DataType <: Data](t: DataType): T

  def name: String

}

//maybe unnecessary
abstract class NumericFeature extends Feature[Double]

abstract class FactorFeature extends Feature[Int] {
  def decode(level: Int): String

  def nLevels: Int

  def dummies: Array[DummyFeature] = {
    (0 until nLevels) map { i => new DummyFeature(this, i, decode(i)) } toArray
  }

}

class NumericColumnFeature(val variable: NumericVariable, val columnIndex: Int) extends NumericFeature {
  def apply[DataType <: Data](t: DataType): Double = t(columnIndex)

  def name = variable.name
}

class FactorColumnFeature(val variable: FactorVariable, val columnIndex: Int) extends FactorFeature {
  override def apply[DataType <: Data](t: DataType): Int = t(columnIndex).toInt

  override def name: String = variable.name

  def decode(level: Int): String = variable.decode(level)

  override def nLevels: Int = variable.nLevels
}

class DummyFeature(val factor: FactorFeature, level: Int, decodedLevel: String) extends NumericFeature {
  override def apply[DataType <: Data](t: DataType): Double = if (factor(t) == level) 1.0 else 0.0

  override def name: String = factor.name + "_" + decodedLevel

}

class NumericAsFactor[DT <: Data](dat: DataFrame[DT], variable: NumericVariable, columnIndex: Int) extends FactorFeature {
  val levelMap = dat.view.map(t => t(columnIndex)).distinct.zipWithIndex.toMap
  val inverse = levelMap.map(_.swap)

  override def decode(level: Int): String = inverse.get(level) match {
    case Some(x) => x.toString
    case None => throw new NoSuchElementException("Unknown level ! ")
  }

  override def nLevels: Int = levelMap.size

  override def name: String = s"factor(${variable.name}})"

  override def apply[DataType <: Data](t: DataType): Int = levelMap(t(columnIndex))
}

class ConstantFeature(val value: Double) extends NumericFeature {
  override def apply[DataType <: Data](t: DataType): Double = value

  override def name: String = value.toString
}

class MappedNumericFeature(source: NumericFeature,
                           op: Double => Double,
                           opName: String) extends NumericFeature {
  override def apply[DataType <: Data](t: DataType): Double = op(source(t))

  override def name: String = s"$opName(${source.name})"
}

class BinaryMappedNumericFeatures(first: NumericFeature,
                                  second: NumericFeature,
                                  op: (Double, Double) => Double,
                                  opName: String,
                                  infix: Boolean) extends NumericFeature {
  override def apply[DataType <: Data](t: DataType): Double = ???

  override def name: String = if (infix) {
    s"${first.name} $opName ${second.name}"
  } else {
    s"$opName(${first.name},${second.name})"
  }
}



