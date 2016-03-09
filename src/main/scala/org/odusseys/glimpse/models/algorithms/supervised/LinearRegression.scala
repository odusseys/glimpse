package org.odusseys.glimpse.models.algorithms.supervised

import breeze.linalg.{DenseVector, inv, DenseMatrix}
import org.odusseys.glimpse.data.{DataFrame, Data}
import LinearRegression._
import org.odusseys.glimpse.models.formulas.Formula
import org.odusseys.glimpse.models.optimization.AdagradOptimizer

/**
 * Created by umizrahi on 08/03/2016.
 */
class LinearRegression(formula: Formula,
                       method: Method = Newton,
                       maxIterations: Int = 100,
                       initialIntercept: Double = 0.0,
                       intialCoefficient: Seq[Double] = null) {


  def train[DataType <: Data](data: DataFrame[DataType]): LinearRegressionModel = {
    val reader = formula.decodeFor(data)
    require(reader.signature._1 == 1, "Formula should have a single response !")
    method match {
      case SGD => trainAdagrad(data, reader.variables, reader.responses(0))
      case _ => throw new NotImplementedError()
    }
  }

  private def trainAdagrad[DataType <: Data](data: DataFrame[DataType], variables: Array[DataType => Double], response: DataType => Double): LinearRegressionModel = {
    val learner = new AdagradLinearRegression(initialIntercept, variables, response)
    learner.train(data, maxIterations)
  }

  private def trainNewton[DataType <: Data](data: DataFrame[DataType], variables: Array[DataType => Double], response: DataType => Double): LinearRegressionModel = {

    val design = DenseMatrix.zeros[Double](data.size, variables.length + 1)
    var i = 0
    data.foreach { l =>
      design(i, 0) = 1.0
      variables.indices.foreach { j =>
        design(i, j + 1) = variables(j)(l)
      }
      i = i + 1
    }
    val y = DenseVector(data.map(response))
    val inverse = inv(design.t * design)
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

  case object SGD extends Method

  case object LBFGS extends Method

  case object Newton extends Method

  class AdagradLinearRegression[DataType <: Data](initialBaseline: Double,
                                                  variables: Array[DataType => Double],
                                                  response: DataType => Double) extends AdagradOptimizer {
    val baseline = scalarParm(initialBaseline)
    val coefficients = vectorParm(variables.length)

    def train(data: DataFrame[DataType], iterations: Int) = {
      1 to iterations foreach { _ => data.foreach { l =>
        val delta = predict(l) - response(l)
        baseline.updateWithGradient(delta)
        variables.indices.foreach { i =>
          coefficients.updateWithGradient(i, delta * variables(i)(l))
        }
      }
      }
      new LinearRegressionModel(baseline, coefficients.value.toVector)
    }

    def predict(l: DataType) = {
      baseline + variables.indices.map(i => variables(i)(l) * coefficients(i)).sum
    }

  }

}
