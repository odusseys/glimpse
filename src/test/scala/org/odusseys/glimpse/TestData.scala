package org.odusseys.glimpse

import org.odusseys.glimpse.data.DataFrame

import scala.util.Random

/**
 * Created by umizrahi on 09/03/2016.
 */
object TestData {

  def clusteringData(nCluster: Int = 3,
                     nVariables: Int = 5,
                     nSamples: Int = 1000,
                     seed: Random = new Random()) = {
    require(nCluster > 0)
    val centroids = Array.fill(nCluster, nVariables)(2 * seed.nextGaussian())
    val sds = Array.fill(nCluster, nVariables)(seed.nextDouble())
    val probs = Array.fill(nCluster)(seed.nextDouble())
    val norm = probs.sum
    probs.indices.foreach { i => probs(i) = probs(i) / norm }

    def generateCluster() = {
      val x = seed.nextDouble()
      var c = 0.0
      var i = -1
      do {
        i = i + 1
        c = c + probs(i)
      } while (c < x)
      i
    }

    def generate() = {
      val cluster = generateCluster()
      val x = Array.fill(nVariables)(seed.nextGaussian())
        .zip(sds(cluster))
        .map { case (u, v) => u * v }
        .zip(centroids(cluster))
        .map { case (u, v) => u + v }
      x.toSeq
    }
    DataFrame((1 to nSamples) map { _ => generate() })
  }

  def linearCoefs(nVariables: Int) = (0 until nVariables).map(i => 2 * (i % 2) - 1)

  val linearBaseline = 1

  def linearData(nVariables: Int = 5,
                 nSamples: Int = 1000,
                 seed: Random = new Random()) = {
    val names = (0 until nVariables).map { i => "x" + i }.toSeq ++ Seq("y")
    val baseline = linearBaseline
    val coef = linearCoefs(nVariables)
    def generate() = {
      val x = (0 until nVariables).map(i => seed.nextGaussian())
      val y = baseline + (x, coef).zipped.map(_ * _).sum
      x ++ Seq(y)
    }
    DataFrame((1 to nSamples) map { _ => generate() }, names)
  }


}
