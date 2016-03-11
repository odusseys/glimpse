package org.odusseys.glimpse.models.algorithms.unsupervised

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import org.odusseys.glimpse.data.{DataFrame, Data}
import org.odusseys.glimpse.models.formulas.Formula

import scala.util.Random

/**
 * Created by umizrahi on 10/03/2016.
 */
class GaussianMixtureClustering(formula: Formula = " ~ .", k: Int, iterations: Int = 100) {

  private def initializeClassic(k: Int, n: Int) = {
    val r = new Random()
    val s = Array.fill(k, n, n)(0.0)
    for (t <- 0 until k; i <- 0 until n) s(t)(i)(i) = r.nextDouble()
    (Array.fill(k)(1.0 / k), Array.fill(k, n)(r.nextGaussian()), s)
  }

  def train[T <: Data](data: DataFrame[T]) = {
    val reader = formula.decodeFor(data)
    val variables = reader.variables
    val n = variables.length

    val (p, means, sds) = initializeClassic(k, n)
    val (pTemp, meansTemp, sdsTemp) = (
      Array.ofDim[Double](k),
      Array.ofDim[Double](k, n),
      Array.ofDim[Double](k, n, n)
      )

    val distributions = Array.ofDim[MultivariateGaussian](k)

    def computeDistributions() = {
      (0 until k) foreach { t =>
        val m = new DenseVector[Double](means(t))
        val s = new DenseMatrix[Double](n, n)
        for (i <- 0 until n) {
          for (j <- 0 until n) {
            s(i, j) = sds(t)(i)(j)
          }
        }
        distributions(t) = new MultivariateGaussian(m, s)
      }
    }

    def resetTemp() = {
      for (t <- 0 until k) {
        pTemp(t) = 0.0
        for (i <- 0 until n) {
          meansTemp(t)(i) = 0.0
          for (j <- 0 until n) {
            sdsTemp(t)(i)(j) = 0.0
          }
        }
      }
    }

    def update() = {
      for (t <- 0 until k) {
        p(t) = pTemp(t)
        for (i <- 0 until n) {
          means(t)(i) = meansTemp(t)(i)
          for (j <- 0 until n) {
            sds(t)(i)(j) = sdsTemp(t)(i)(j)
          }
        }
      }
    }

    val tauArray = Array.ofDim[Double](k)

    //todo : maybe code the pdf ourselves, or cache the data, since there is a lot of object creation going on here
    def taus(l: T) = {
      var sum = 0.0
      for (t <- 0 until k) {
        val u = p(t) * distributions(t).pdf(new DenseVector[Double](variables.map(v => v(l))))
        tauArray(t) = u
        sum = sum + u
      }
      for (t <- 0 until k) {
        tauArray(t) = tauArray(t) / sum
      }
      tauArray
    }

    def compute() = {
      data.foreach { l =>
        val tau = taus(l)
        for (t <- 0 until k) {
          pTemp(t) = pTemp(t) + tau(t)
          for (i <- 0 until n) {
            val vi = variables(i)(l)
            meansTemp(t)(i) = meansTemp(t)(i) + tau(t) * vi
            for (j <- 0 until n) {
              val u = (vi - means(t)(i)) * (variables(j)(l) - means(t)(j))
              sdsTemp(t)(i)(j) = sdsTemp(t)(i)(j) + tau(t) * u
            }
          }
        }
      }
      for (t <- 0 until k) {
        for (i <- 0 until n) {
          meansTemp(t)(i) = meansTemp(t)(i) / pTemp(t)
          for (j <- 0 until n) {
            sdsTemp(t)(i)(j) = sdsTemp(t)(i)(j) / pTemp(t)
          }
        }
        pTemp(t) = pTemp(t) / data.size
      }
    }

    def iteration() = {
      computeDistributions()
      compute()
      update()
      resetTemp()
    }

    for (i <- 1 to iterations) iteration()

  }

}
