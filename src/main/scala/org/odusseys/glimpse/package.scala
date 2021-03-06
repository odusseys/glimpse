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

    def **(exp: Double): Double = math.pow(i, exp)

    def **(exp: Int): Int = (math.pow(i, exp) + 0.5) toInt
  }

  implicit class DoubleAdditions(d: Double) {
    def **(exp: Double): Double = math.pow(d, exp)
  }


  /* Operations to write sums and products in mathematical-ish notation*/

  def sum[S, T](range: Traversable[S])(value: S => T)(implicit n: Numeric[T]) = {
    range.map(value).sum
  }

  def product[S, T](range: Traversable[S])(value: S => T)(implicit n: Numeric[T]) = {
    range.map(value).product
  }

  /*Operations for numeric traversable collections*/
  implicit class TraversableLikeOperations[T, Repr[T]](coll: TraversableLike[T, Repr[T]]) {

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

    def cartesian[S, Repr2[S], That](other: TraversableLike[S, Repr2[S]])(implicit cbf: CanBuildFrom[Repr[T], (T, S), That],
                                                                          cbf2: CanBuildFrom[Repr2[S], (T, S), TraversableLike[(T, S), Repr2[(T, S)]]]): That = {
      coll.flatMap(c => other.map(o => (c, o)))
    }

  }

  implicit class TraversableOperations[T](coll: Traversable[T]) {
    def mean(implicit n: Numeric[T]) = {
      val (x, m) = coll.foldLeft((0.0, 0))((u, v) => (u._1 + n.toDouble(v), u._2 + 1))
      x / m
    }

    def variance(implicit n: Numeric[T]) = {
      val (x, xx, m) = coll.foldLeft((0.0, 0.0, 0))((u, v) => {
        val x = n.toDouble(v)
        (u._1 + x, u._2 + x * x, u._3 + 1)
      })
      val mean = x / m
      math.max(xx - mean * mean, 0.0)
    }

    def sd(implicit n: Numeric[T]) = {
      math.sqrt(variance)
    }


  }

}
