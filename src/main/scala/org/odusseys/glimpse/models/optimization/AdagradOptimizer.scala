package org.odusseys.glimpse.models.optimization

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created by umizrahi on 09/03/2016.
 */
class AdagradOptimizer(val learningRate: Double = 0.1) {

  protected val scalarParms = new ArrayBuffer[ScalarParm]
  protected val vectorParms = new ArrayBuffer[VectorParm]
  protected val matrixParms = new ArrayBuffer[MatrixParm]

  protected def scalarParm(initialValue: Double = 0.0): ScalarParm = {
    val p = new ScalarParm(initialValue)
    scalarParms.append(p)
    p
  }

  protected def vectorParm(initialValue: Array[Double]): VectorParm = {
    val p = new VectorParm(initialValue)
    vectorParms.append(p)
    p
  }

  protected def vectorParm(n: Int): VectorParm = vectorParm(Array.fill(n)(0.0))

  protected def matrixParm(initialValue: Array[Array[Double]]) = {
    val p = new MatrixParm(initialValue)
    matrixParms.append(p)
    p
  }

  class ScalarParm private[AdagradOptimizer](val initialValue: Double) {
    var value = initialValue
    var squares = 1.0

    def updateWithGradient(grad: Double) = {
      squares = squares + grad * grad
      value = value - learningRate * grad / Math.sqrt(squares)
    }

    def reset() = value = initialValue

  }

  class VectorParm private[AdagradOptimizer](val initialValue: Array[Double]) {

    val value = initialValue.clone()
    val squares = Array.fill(value.length)(1.0)

    def updateWithGradient(i: Int, grad: Double) = {
      squares(i) = squares(i) + grad * grad
      value(i) = value(i) - learningRate * grad / Math.sqrt(squares(i))
    }

    def reset() = Array.copy(initialValue, 0, value, 0, value.length)

    def apply(i: Int) = value(i)

  }

  class MatrixParm private[AdagradOptimizer](val initialValue: Array[Array[Double]]) {
    require(initialValue != null && initialValue.length > 0 && initialValue(0).length > 0)
    val value = initialValue.clone()
    val squares = Array.fill(initialValue.length, initialValue(0).length)(1.0)

    def updateWithGradient(i: Int, j: Int, grad: Double) = {
      squares(i)(j) = squares(i)(j) + grad * grad
      value(i)(j) = value(i)(j) - learningRate * grad / Math.sqrt(squares(i)(j))
    }

    def reset() = value.indices.foreach { i =>
      Array.copy(initialValue(i), 0, value(i), 0, value(i).length)
    }

    def apply(i: Int, j: Int) = value(i)(j)

  }

  implicit def scalarToDouble(s: ScalarParm): Double = s.value

  implicit def vectorToArray(v: VectorParm): Array[Double] = v.value

  implicit def matrixToArrays(m: MatrixParm): Array[Array[Double]] = m.value

}

class Test extends AdagradOptimizer {
  val s = scalarParm(1.5)
  println(s.initialValue)
  scalarParms.foreach(s => println(s.initialValue))
}

