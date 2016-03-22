package org.odusseys.glimpse.models.algorithms.supervised.linear

import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.models.algorithms.Method
import org.odusseys.glimpse.models.algorithms.supervised.linear.GLM.Binomial
import org.odusseys.glimpse.models.formulas.Formula

/**
 * Created by umizrahi on 10/03/2016.
 */
class LogisticRegression(formula: Formula,
                         maxIterations: Int = 100,
                         initialIntercept: Double = 0.0,
                         initialCoefficients: Seq[Double] = null) {
  def train[T <: Data](data: DataFrame[T], method: Method) = {
    val glm = new GLM(formula, Binomial, maxIterations, initialIntercept, initialCoefficients).train(data, method)
    new LogisticRegressionModel[T](glm.intercept, glm.coefficients)
  }

}

class LogisticRegressionModel[T <: Data](override val intercept: Double,
                                         override val coefficients: Array[Double])
  extends GLMModel[T](intercept, coefficients, Binomial) {
  def classify(data: T, threshold: Double = 0.5) = {
    if (predict(data) < threshold) 0.0 else 1.0
  }
}