package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
trait Data {
  def indices: Iterator[Int]

  def apply(i: Int): Double
}
