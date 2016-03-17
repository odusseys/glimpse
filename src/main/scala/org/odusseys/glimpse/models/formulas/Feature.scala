package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{Data, Variable}

/**
 * Created by umizrahi on 17/03/2016.
 */
class Feature[T](val variable: Variable, val index: Int){
  def apply[DataType <: Data](t: DataType) = t(index)
  def name = variable.name
}
