package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
trait Data {
  def indices: Iterator[Int]

  def apply(i: Int): Double

  def apply(s: String): Double

  def apply(v: Variable): Double = apply(v.name)

  def size: Int

  def toArray: Array[Double] = (0 until size) map apply toArray

  def contains(i: Int): Boolean

}
