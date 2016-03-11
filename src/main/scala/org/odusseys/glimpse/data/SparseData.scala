package org.odusseys.glimpse.data

import breeze.linalg.SparseVector

/**
 * Created by umizrahi on 08/03/2016.
 */
class SparseData(vec: SparseVector[Double], columnMapping: ColumnMapping) extends Data {

  def this(dat: Map[Int, Double], size: Int, columnMapping: ColumnMapping) = this({
    val indices = dat.keys.toArray.sorted
    new SparseVector[Double](indices, indices.map(dat.apply), size)
  }, columnMapping)

  override def indices: Iterator[Int] = vec.activeKeysIterator

  override def apply(i: Int): Double = vec(i)

  override def size: Int = vec.length

  override def apply(s: String): Double = columnMapping(s)
}

object SparseData {

}
