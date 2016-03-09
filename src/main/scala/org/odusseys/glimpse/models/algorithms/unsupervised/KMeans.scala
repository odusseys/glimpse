package org.odusseys.glimpse.models.algorithms.unsupervised

import org.odusseys.glimpse.data.{DataFrame, Data}
import org.odusseys.glimpse.models.formulas.Formula
import scala.util.Random

/**
 * Created by umizrahi on 09/03/2016.
 */
class KMeans(formula: Formula = " ~ .", k: Int, iterations: Int = 100) {

  import KMeans._

  private def initializeClassic(k: Int, n: Int) = {
    val r = new Random()
    Array.fill(k, n)(r.nextGaussian())
  }

  def train[T <: Data](data: DataFrame[T]) = {
    val reader = formula.decodeFor(data)
    val variables = reader.variables
    val n = variables.length
    val centroids = initializeClassic(k, n)



    def assign(d: T) = {
      centroids.indices.minBy(i => squareDistance(d, centroids(i)))
    }

    //todo : replace with breeze
    val newCentroids = Array.fill(k, n)(0.0)
    val counts = Array.fill(k)(0)

    def clearTemp() = {
      (0 until k) foreach { t =>
        counts(t) = 0
        (0 until n) foreach { i => newCentroids(t)(i) = 0.0 }
      }
    }

    def updateCentroids() = {
      (0 until k) foreach { t =>
        counts(t) = 0
        (0 until n) foreach { i => centroids(t)(i) = newCentroids(t)(i) / counts(t) }
      }
    }

    def iteration() = {
      clearTemp()
      data.foreach { l =>
        val t = assign(l)
        counts(t) = counts(t) + 1
        val cl = newCentroids(t)
        (0 until n).foreach(i => cl(i) = cl(i) + variables(i)(l))
      }
      updateCentroids()
    }

    for (i <- 1 to iterations) iteration()

    new KMeansModel(centroids.map(_.toVector))
  }
}

class KMeansModel(val centroids: IndexedSeq[Vector[Double]]) {

  import KMeans._

  def classify(data: Data) = {
    centroids.indices.minBy(i => squareDistance(data, centroids(i)))
  }
}

object KMeans {
  def squareDistance(d: Data, v: IndexedSeq[Double]) = {
    d.indices.map(i => d(i) - v(i)).map(u => u * u).sum
  }
}
