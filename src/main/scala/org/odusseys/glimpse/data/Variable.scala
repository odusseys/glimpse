package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
sealed abstract class Variable(val name: String)

class NumericVariable(override val name: String) extends Variable(name)

class FactorVariable[T](override val name: String,
                        val processor: FactorProcessor = new FactorProcessor) extends Variable(name) {
  def process(level: String) = processor.process(level)
}

class FactorDummy[T](factorName: String, level: T) extends NumericVariable(factorName + "$" + level) {
  def getFactor = new FactorVariable[T](factorName)
}
