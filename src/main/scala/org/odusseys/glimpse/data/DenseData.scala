package org.odusseys.glimpse.data

import breeze.linalg.DenseVector

/**
 * Created by umizrahi on 08/03/2016.
 */
class DenseData(vec: DenseVector[Double], columnMapping: ColumnMapping) extends Data {

  def this(arr: Seq[Double], columnMapping: ColumnMapping) = this(new DenseVector(arr.toArray), columnMapping)

  override def indices: Iterator[Int] = vec.activeKeysIterator

  override def apply(i: Int): Double = vec(i)

  def size: Int = vec.length

  override def toArray: Array[Double] = vec.toArray

  override def apply(s: String): Double = apply(columnMapping(s))

  override def contains(i: Int): Boolean = i < size
}
