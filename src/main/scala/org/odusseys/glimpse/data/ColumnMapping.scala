package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class ColumnMapping(val columns: Seq[Variable]) {
  require(columns.map(_.name).toSet.size == columns.size, "Cannot have udplicate column names !")
  private val reverse = columns.zipWithIndex.toMap

  private val mapping = reverse.map(_.swap)

  private val nameMapping = columns.map(v => (v.name, reverse(v))).toMap

  def apply(v: Variable) = reverse(v)

  def apply(i: Int) = mapping(i)

  def apply(s: String) = nameMapping(s)

  def get(v: Variable) = reverse.get(v)

  def get(i: Int) = mapping.get(i)

  def get(s: String) = nameMapping.get(s)
}

object ColumnMapping {
  implicit def toIntMap(c: ColumnMapping): Map[Int, Variable] = c.mapping
}
