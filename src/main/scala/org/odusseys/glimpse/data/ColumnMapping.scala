package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class ColumnMapping(private val mapping: Map[Int, Variable]) {
  val reverse = mapping.map(_.swap)

  def apply(i: Variable) = reverse(i)
}

object ColumnMapping {
  implicit def toMap(c: ColumnMapping): Map[Int, Variable] = c.mapping
}
