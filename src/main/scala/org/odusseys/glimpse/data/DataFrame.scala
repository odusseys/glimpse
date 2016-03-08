package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class DataFrame[DataType <: Data](val data: Seq[DataType], val mapping: ColumnMapping) {

  def print() = {
    println(mapping.toSeq.sortBy(_._1).map(_._2.name).mkString("\t"))
    data.foreach { l => println((0 until l.size).map(l.apply).mkString("\t")) }
  }

  def nColumns = mapping.size

  def columns = mapping.columns

}

object DataFrame {
  implicit def toSeq[T <: Data](data: DataFrame[T]): Seq[T] = data.data
}
