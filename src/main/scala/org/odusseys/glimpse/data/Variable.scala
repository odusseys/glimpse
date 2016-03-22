package org.odusseys.glimpse.data

import scala.collection.mutable

/**
 * Created by umizrahi on 04/03/2016.
 */
sealed abstract class Variable(val name: String) {
  def decode(value: Double): String

  def continuous: Boolean
}

class NumericVariable(override val name: String) extends Variable(name) {
  override def decode(value: Double) = value.toString

  override def continuous = true
}

class FactorVariable(override val name: String) extends Variable(name) {

  private val mapping = new mutable.HashMap[String, Int]()
  private val inverseMapping = new mutable.HashMap[Int, String]()

  def process(level: String) = {
    if (!mapping.contains(level)) {
      val i = mapping.size
      mapping.put(level, i)
      inverseMapping.put(i, level)
      i
    } else {
      mapping(level)
    }
  }

  override def continuous = false

  def encodedLevel(level: String) = mapping(level)

  def decodedLevel(level: Int) = inverseMapping(level)

  def nLevels = mapping.size

  def levels = inverseMapping.toSeq.sortBy(_._1).map(_._2)

  override def decode(value: Double): String = inverseMapping(value.toInt)
}

class FactorDummy[T](factorName: String, level: String) extends NumericVariable(factorName + "$" + level) {
  def getFactor = new FactorVariable(factorName)
}
