package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
trait Data {
  def indices: Traversable[Int]

  def apply(i: Int): Double
}
