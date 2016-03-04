package org.odusseys

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

/**
 * Created by umizrahi on 04/03/2016.
 */
package object glimpse {

  implicit class IntAdditions(i: Int) {
    def times(action: => Unit): Unit = {
      1 to i foreach (_ => action)
    }
  }

  /* Operations to write sums and products in mathematical-ish notation*/

  def sum[S, T](range: Traversable[S])(value: S => T)(implicit n: Numeric[T]) = {
    range.map(value).sum
  }

  def product[S, T](range: Traversable[S])(value: S => T)(implicit n: Numeric[T]) = {
    range.map(value).product
  }

  /*Operations for numeric traversable collections*/
  implicit class TraversableOperations[T, Repr[T]](coll: TraversableLike[T, Repr[T]]) {

    private def applyNumericFunction[That](f: Double => Double)(implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = {
      coll.map(u => f(num.toDouble(u)))
    }

    def square[That](implicit cbf: CanBuildFrom[Repr[T], T, That], num: Numeric[T]): That = {
      coll.map(u => num.times(u, u))
    }

    def pow[That](exponent: Double)(implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = {
      coll.map(u => math.pow(num.toDouble(u), exponent))
    }

    def inverse[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(1.0 / _)

    def sqrt[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.sqrt)

    def log[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.log)

    def exp[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.exp)

    def cos[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.cos)

    def sin[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.sin)

    def cosh[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.cosh)

    def sinh[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.sinh)

    def tan[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.tan)

    def tanh[That](implicit cbf: CanBuildFrom[Repr[T], Double, That], num: Numeric[T]): That = applyNumericFunction(math.tanh)
  }


}
