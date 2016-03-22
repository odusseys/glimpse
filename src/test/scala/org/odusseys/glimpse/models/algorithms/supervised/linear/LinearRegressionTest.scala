package org.odusseys.glimpse.models.algorithms.supervised.linear

import org.odusseys.glimpse.TestData
import org.odusseys.glimpse.models.algorithms.SGD
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 09/03/2016.
 */
class LinearRegressionTest extends FunSuite {
  val data = TestData.linearData()
  test("test predictions with Newton's method") {
    val model = new LinearRegression("y ~ .").train(data)
    val rmse = math.sqrt(data.map(l => l("y") - model.predict(l)).map(u => u * u).sum / data.size)
    assert(rmse < 0.05)
  }

  test("test predictions with SGD method") {
    val model = new LinearRegression("y ~ .").train(data, SGD)
    val rmse = math.sqrt(data.map(l => l("y") - model.predict(l)).map(u => u * u).sum / data.size)
    assert(rmse < 0.05)
  }
}
