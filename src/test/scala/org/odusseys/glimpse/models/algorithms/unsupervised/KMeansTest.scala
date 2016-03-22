package org.odusseys.glimpse.models.algorithms.unsupervised

import org.odusseys.glimpse.TestData
import org.odusseys.glimpse.data.DenseData
import org.scalatest.FunSuite

import scala.util.Random

/**
 * Created by umizrahi on 11/03/2016.
 */
class KMeansTest extends FunSuite {
  test("should find 2 clusters") {
    val data = TestData.clusteringData(nClusters = 2, nVariables = 3)
    val model = new KMeans(2).train(data)

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

    val modelError = error((arr: DenseData) => model.centroids(model.classify(arr)).toArray)
    val randomError = (1 to 1000).map(_ => error(randomClustering())).sum / 1000.0
    assert(modelError < 0.5 * randomError)
  }
}
