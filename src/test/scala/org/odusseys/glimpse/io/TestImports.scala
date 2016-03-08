package org.odusseys.glimpse.io

import org.odusseys.glimpse.data.{FactorVariable, NumericVariable}
import org.scalatest.FunSuite

/**
 * Created by umizrahi on 08/03/2016.
 */
class TestImports extends FunSuite {

  val data = Import.fromCSV("src/test/resources/data.csv")

  test("imported csv should have expected columns") {
    assert(data.nColumns == 3)
    val cols = data.columns
    assert(cols.length == 3)
    assert(cols.map(_.name).zip(List("a", "b", "c")).forall { case (u, v) => u.equals(v) })
    assert(cols(0).isInstanceOf[NumericVariable])
    assert(cols(1).isInstanceOf[NumericVariable])
    assert(cols(2).isInstanceOf[FactorVariable])
    val f = cols(2).asInstanceOf[FactorVariable]
    assert(f.nLevels == 2)
    assert(f.levels.zip(List("x", "1")).forall { case (u, v) => u == v })
  }
}
