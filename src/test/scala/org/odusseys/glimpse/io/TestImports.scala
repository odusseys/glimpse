package org.odusseys.glimpse.io

import org.odusseys.glimpse.data.{FactorColumn, NumericColumn}
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
    assert(cols(0).isInstanceOf[NumericColumn])
    assert(cols(1).isInstanceOf[NumericColumn])
    assert(cols(2).isInstanceOf[FactorColumn])
    val f = cols(2).asInstanceOf[FactorColumn]
    assert(f.nLevels == 2)
    assert(f.levels.zip(List("x", "1")).forall { case (u, v) => u == v })
  }
}
