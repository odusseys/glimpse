package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
trait FilterableData extends Data {
  def filterColumns(filter: Int => Boolean, remapping: Int => Int)
}
