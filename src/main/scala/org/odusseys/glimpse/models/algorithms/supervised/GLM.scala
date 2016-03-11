package org.odusseys.glimpse.models.algorithms.supervised

import breeze.numerics.{log, exp}
import org.odusseys.glimpse.data.{DataFrame, Data}
import org.odusseys.glimpse.models.algorithms.{SGD, Method}
import org.odusseys.glimpse.models.formulas.Formula
import org.odusseys.glimpse.models.optimization.AdagradOptimizer

/**
 * Created by umizrahi on 10/03/2016.
 */

import GLM._

class GLM(formula: Formula,
          family: Family = Gaussian,
          maxIterations: Int = 100,
          initialIntercept: Double = 0.0,
          initialCoefficients: Seq[Double] = null) {


  def train[T <: Data](data: DataFrame[T],
                       method: Method = SGD): GLMModel[T] = {
    val reader = formula.decodeFor(data)
    require(Set(1, 2).contains(reader.signature._1), "Formula must have at least one response (two if using weights).")
    val response = reader.responses(0)
    val weight = if (reader.signature._1 == 2) reader.responses(1) else (t: T) => 1.0
    val variables = reader.variables
    method match {
      case SGD => trainWithAdagrad(data, weight, response, variables)
      // case LBFGS => trainWithLBFGS(data, weight, response, variables)
    }
  }

  def trainWithAdagrad[DataType <: Data](data: DataFrame[DataType],
                                         weight: (DataType) => Double,
                                         response: (DataType) => Double,
                                         variables: Array[DataType => Double]) = {
    new AdagradGLM(initialIntercept, initialCoefficients, family, variables, response, weight).train(data, maxIterations)
  }
}

class GLMModel[DataType <: Data](val intercept: Double, val coefficients: Array[Double], family: Family) {

  def predictRaw(data: DataType) = {
    intercept + data.indices.map(i => data(i) * coefficients(i)).sum
  }

  def predict(data: DataType) = {
    family.link(predictRaw(data))
  }
}

object GLM {


  sealed trait Family {
    def link(t: Double): Double

    def inverseLink(t: Double): Double

    def loss(prediction: Double, label: Double): Double

    def saturatedLoss(label: Double): Double

    def normalization(prediction: Double, label: Double): Double

  }

  object Binomial extends Family {
    override def link(t: Double) = 1.0 / (1.0 + exp(-t))

    override def inverseLink(t: Double): Double = log(t / (1.0 - t))

    override def loss(prediction: Double, label: Double): Double =
      label * log(prediction) + (1 - label) * log(1 - prediction)

    override def saturatedLoss(label: Double): Double = 0.0

    override def normalization(prediction: Double, label: Double): Double = 1
  }

  object Gaussian extends Family {
    override def link(t: Double): Double = t

    override def inverseLink(t: Double): Double = t

    override def loss(prediction: Double, label: Double): Double = {
      val u = prediction - label
      u * u
    }

    override def saturatedLoss(label: Double): Double = 0.0

    override def normalization(prediction: Double, label: Double): Double = 1.0
  }

  object Poisson extends Family {
    override def link(t: Double): Double = exp(t)

    override def inverseLink(t: Double): Double = log(t)

    override def loss(prediction: Double, label: Double): Double = label * log(prediction) - prediction

    override def saturatedLoss(label: Double): Double = label * (log(label) - 1.0)

    override def normalization(prediction: Double, label: Double): Double = 1.0 / prediction
  }

  object LogNormal extends Family {
    override def link(t: Double): Double = exp(t)

    override def saturatedLoss(label: Double): Double = 0.0

    override def inverseLink(t: Double): Double = log(t)

    override def loss(prediction: Double, label: Double): Double = {
      val u = log(prediction) - log(label)
      u * u
    }

    override def normalization(prediction: Double, label: Double): Double = 1.0
  }

  class AdagradGLM[DataType <: Data](initialBaseline: Double,
                                     initialCoefficients: Seq[Double],
                                     family: Family,
                                     variables: Array[DataType => Double],
                                     response: DataType => Double,
                                     weight: DataType => Double) extends AdagradOptimizer {
    val baseline = scalarParm(initialBaseline)
    val coefficients = {
      if (initialCoefficients == null) {
        vectorParm(variables.length)
      } else {
        vectorParm(initialCoefficients.toArray)
      }
    }

    def train(data: DataFrame[DataType], iterations: Int) = {
      1 to iterations foreach { _ => data.foreach { l =>
        val p = predict(l)
        val y = response(l)
        val delta = weight(l) * (p - y) * family.normalization(p, y)
        baseline.updateWithGradient(delta)
        variables.indices.foreach { i =>
          coefficients.updateWithGradient(i, delta * variables(i)(l))
        }
      }
      }
      new GLMModel[DataType](baseline, coefficients, family)
    }

    def predict(l: DataType) = {
      family.link(baseline + variables.indices.map(i => variables(i)(l) * coefficients(i)).sum)
    }

  }

}
