package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
sealed abstract class Variable(name: String)

case class NumericVariable(name: String) extends Variable(name)

case class FactorVariable[T](name: String) extends Variable(name)

case class FactorDummy[T](factorName: String, level: T) extends FactorVariable(factorName + "$" + level) {
  def getFactor = FactorVariable[T](factorName)
}
