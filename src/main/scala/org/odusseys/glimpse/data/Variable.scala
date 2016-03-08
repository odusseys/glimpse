package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
sealed abstract class Variable(val name: String)

case class NumericVariable(override val name: String) extends Variable(name)

case class FactorVariable[T](override val name: String) extends Variable(name)

case class FactorDummy[T](factorName: String, level: T) extends Variable(factorName + "$" + level) {
  def getFactor = new FactorVariable[T](factorName)
}
