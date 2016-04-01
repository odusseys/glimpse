package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.TestData
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 15/03/2016.
 */
class DecisionTreeTest extends FunSuite {

  val data = TestData.linearData()

  test("without weights") {
    val model = new DecisionTree("y = *", depth = 7, minWeightsPerNode = 100).train(data)
    data.foreach(l => println(l("y") + " " + model.predict(l)))
    println(model.nNodes)
    println(model)
  }
}
