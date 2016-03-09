package org.odusseys.glimpse

/**
 * Created by umizrahi on 04/03/2016.
 */
object Test {
  def main(args: Array[String]) {
    import breeze.linalg._

    val a = new DenseMatrix[Double](3, 3)
    val b = new DenseMatrix[Double](3, 3, (1 to 9) map (_.toDouble) toArray)
    println(a)
    println(b)
    val c = (b * b)
    println(c.getClass)
    println(c)

  }


}
