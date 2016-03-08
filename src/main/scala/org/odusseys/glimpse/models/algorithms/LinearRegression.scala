package org.odusseys.glimpse.models.algorithms

import org.odusseys.glimpse.data.Data
import org.odusseys.glimpse.models.algorithms.LinearRegression._

/**
 * Created by umizrahi on 08/03/2016.
 */
class LinearRegression(method: Method = Newton,
                       maxIterations: Int = 100,
                       initialIntercept: Double = 0.0,
                       intialCoefficient: Seq[Double] = null) {

  def train: LinearRegressionModel = {
    method match {
      case Adagrad => trainAdagrad
      case _ => throw new NotImplementedError()
    }
  }

  private def trainAdagrad: LinearRegressionModel = {
    null
  }

}

class LinearRegressionModel(val intercept: Double, val coefficients: Vector[Double]) {
  def predict(d: Data) = intercept + d.indices.map(i => d(i) * coefficients(i)).sum

  def predict(a: Array[Double]) = intercept + a.indices.map(i => a(i) * coefficients(i)).sum

  def predict(a: Seq[Double]) = {
    require(a.size == coefficients.size, "Coefficients and example data should have same size.")
    intercept + (a, coefficients).zipped.map(_ * _).sum
  }
}

object LinearRegression {

  sealed trait Method

  case object Adagrad extends Method

  case object LBFGS extends Method

  case object Newton extends Method

}
