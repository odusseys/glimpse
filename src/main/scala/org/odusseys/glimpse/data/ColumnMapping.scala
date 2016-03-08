package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class ColumnMapping(val columns: Array[Variable]) {
  private val reverse = columns.zipWithIndex.toMap

  private val mapping = reverse.map(_.swap)

  def apply(v: Variable) = reverse(v)

  def apply(i: Int) = mapping(i)
}

object ColumnMapping {
  implicit def toIntMap(c: ColumnMapping): Map[Int, Variable] = c.mapping
}
