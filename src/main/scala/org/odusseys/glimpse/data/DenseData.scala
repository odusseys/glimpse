package org.odusseys.glimpse.data

import breeze.linalg.DenseVector

/**
 * Created by umizrahi on 08/03/2016.
 */
class DenseData(vec: DenseVector[Double]) extends Data {
  override def indices: Iterator[Int] = vec.activeKeysIterator

  override def apply(i: Int): Double = vec(i)
}
