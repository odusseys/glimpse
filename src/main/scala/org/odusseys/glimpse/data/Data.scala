package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
trait Data {
  def indices: Iterator[Int]

  def apply(i: Int): Double

  def size: Int

  def toArray: Array[Double] = (0 until size) map apply toArray

}
