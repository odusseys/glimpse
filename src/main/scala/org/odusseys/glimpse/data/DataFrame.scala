package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class DataFrame[DataType <: Data](private val data: Seq[DataType], mapping: ColumnMapping) {
  def getMapping = mapping
}

object DataFrame {
  implicit def toSeq[T](data: DataFrame[T]): Seq[T] = data.data
}
