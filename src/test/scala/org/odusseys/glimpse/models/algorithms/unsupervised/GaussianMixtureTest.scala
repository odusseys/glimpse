package org.odusseys.glimpse.models.algorithms.unsupervised

import org.odusseys.glimpse.TestData
import org.odusseys.glimpse.data.DenseData
import org.scalatest.FunSuite

import scala.util.Random

/**
 * Created by umizrahi on 11/03/2016.
 */
class GaussianMixtureTest extends FunSuite {
  test("should find 2 clusters") {
    val data = TestData.clusteringData(nClusters = 2, nVariables = 3)
    val model = new GaussianMixture(k = 2).train(data)

    def error(clustering: DenseData => Array[Double]) = {
      data.view.map { l =>
        l.toArray.zip(clustering(l)).map { case (u, v) => u - v }.map(u => u * u).sum
      }.sum
    }

    val r = new Random

    def randomClustering() = {
      val a = data(r.nextInt(data.size)).toArray
      val b = data(r.nextInt(data.size)).toArray
      (arr: DenseData) => if (r.nextBoolean()) a else b
    }

    val modelError = error((arr: DenseData) => model.means(model.classify(arr)))
    val randomError = (1 to 1000).map(_ => error(randomClustering())).sum / 1000.0

    data.foreach(l => println(l.indices.map(l.apply).mkString("\t")))

    println("means : ")
    model.means.foreach(l => println(l.mkString("\t")))

    assert(modelError < 0.5 * randomError)
  }

}
