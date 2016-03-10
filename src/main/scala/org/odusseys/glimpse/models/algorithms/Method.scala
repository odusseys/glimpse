package org.odusseys.glimpse.models.algorithms

/**
 * Created by umizrahi on 10/03/2016.
 */
trait Method

case object SGD extends Method

case object LBFGS extends Method