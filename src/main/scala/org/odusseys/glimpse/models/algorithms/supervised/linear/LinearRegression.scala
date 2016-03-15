package org.odusseys.glimpse.models.algorithms.supervised.linear

import breeze.linalg._
import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.models.algorithms.Method
import org.odusseys.glimpse.models.algorithms.supervised.linear.GLM.Gaussian
import org.odusseys.glimpse.models.algorithms.supervised.linear.LinearRegression._
import org.odusseys.glimpse.models.formulas.Formula

/**
 * Created by umizrahi on 08/03/2016.
 */
class LinearRegression(formula: Formula,
                       maxIterations: Int = 100,
                       initialIntercept: Double = Double.NaN,
                       initialCoefficients: Seq[Double] = null) {


  def train[DataType <: Data](data: DataFrame[DataType], method: Method = Newton) = {
    val reader = formula.decodeFor(data)
    require(reader.signature._1 == 1, "Formula should have a single response !")
    val variables = reader.numericVariables
    val response = reader.numericResponses(0)
    method match {
      case Newton => trainNewton(data, variables, response)
      case _ =>
        val initBaseline = if (initialIntercept.isNaN) {
          data.map(l => response(l)).sum / data.size
        } else {
          initialIntercept
        }
        val glm = new GLM(formula, Gaussian, maxIterations, initBaseline, initialCoefficients)
          .train(data, method)
        new LinearRegressionModel(glm.intercept, glm.coefficients, variables)
    }
  }

  private def trainNewton[DataType <: Data](data: DataFrame[DataType], variables: Array[DataType => Double], response: DataType => Double) = {

    val design = DenseMatrix.zeros[Double](data.size, variables.length + 1)
    var i = 0
    data.foreach { l =>
      design(i, 0) = 1.0
      variables.indices.foreach { j =>
        design(i, j + 1) = variables(j)(l)
      }
      i = i + 1
    }
    val y = new DenseMatrix[Double](data.size, 1, data.map(response).toArray)
    val inverse = inv(design.t * design)
    val result = (inverse * design.t).asInstanceOf[DenseMatrix[Double]] * y
    val c = result.asInstanceOf[DenseMatrix[Double]].toArray
    new LinearRegressionModel(c(0), c.drop(1), variables)
  }

}

class LinearRegressionModel[T <: Data](val intercept: Double,
                                       val coefficients: Array[Double],
                                       variables: Array[T => Double]) {
  def predict(d: T) = intercept + variables.indices.map(i => variables(i)(d) * coefficients(i)).sum

  def predict(a: Array[Double]) = intercept + a.indices.map(i => a(i) * coefficients(i)).sum

  def predict(a: Seq[Double]) = {
    require(a.size == coefficients.size, "Coefficients and example data should have same size.")
    intercept + (a, coefficients).zipped.map(_ * _).sum
  }
}

object LinearRegression {

  case object Newton extends Method

}
