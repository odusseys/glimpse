package org.odusseys.glimpse.models

import breeze.optimize.MaxIterations
import org.odusseys.glimpse.data.Data
import org.odusseys.glimpse.models.LinearRegression._

/**
 * Created by umizrahi on 08/03/2016.
 */
class LinearRegression(method: Method = Newton,
                       maxIterations: Int = 100) {
  def train = {

  }

  private def trainWith

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
