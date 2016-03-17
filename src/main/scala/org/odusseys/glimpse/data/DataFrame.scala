package org.odusseys.glimpse.data

import scala.util.Random

/**
 * Created by umizrahi on 04/03/2016.
 */
class DataFrame[DataType <: Data](val data: IndexedSeq[DataType], val mapping: ColumnMapping) {

  def print() = {
    println(mapping.toSeq.sortBy(_._1).map(_._2.name).mkString("\t"))
    data.foreach { l => println((0 until l.size).map(l.apply).mkString("\t")) }
  }

  def nColumns = mapping.size

  def columns = mapping.columns

  def baggingSample(fraction: Double = 1.0, seed: Random = new Random()) = {
    val sampled = (1 to (data.size * fraction).toInt)
      .map { i => data(seed.nextInt(data.size)) }
    new DataFrame(sampled, mapping)
  }

  def splitTwoWay(testFraction: Double, seed: Random = new Random()): (DataFrame[DataType], DataFrame[DataType]) = {
    val cutoff = (data.size * (1.0 - testFraction)).toInt
    val (train, test) = seed.shuffle(data).splitAt(cutoff)
    (new DataFrame(train, mapping), new DataFrame(test, mapping))
  }

  def splitThreeWay(validationFraction: Double,
                    testFraction: Double,
                    seed: Random = new Random()): (DataFrame[DataType], DataFrame[DataType], DataFrame[DataType]) = {
    require(testFraction + validationFraction < 1, "Sum of test and validation fraction cnanot be higher than 1.")
    val cutoff = (data.size * (1.0 - testFraction)).toInt
    val (train, testval) = seed.shuffle(data).splitAt(cutoff)
    val cutoff2 = (validationFraction / (validationFraction + testFraction)) * testval.size
    val (validate, test) = testval.splitAt(cutoff2.toInt)
    (new DataFrame(train, mapping), new DataFrame(validate, mapping), new DataFrame(test, mapping))
  }

  def crossValidationFolds(nFolds: Int, seed: Random = new Random()) = {
    val groupSize = this.size / nFolds
    val folds = data.grouped(groupSize).toArray
    folds.indices.map(i => (
      new DataFrame(folds.indices.filter(_ != i).flatMap(folds(_)), mapping),
      new DataFrame(folds(i), mapping)
      )
    ).toArray
  }

}

object DataFrame {

  implicit def asIndexedSeq[T <: Data](data: DataFrame[T]): IndexedSeq[T] = data.data

  def apply(data: Iterable[Seq[Double]], columnNames: Seq[String]): DataFrame[DenseData] = {
    val n = columnNames.size
    require(data.forall(_.size == n), "All sub-collections in the provided collection must have the same size !")
    val variables = (0 until n) map { i => new NumericVariable(columnNames(i)) } toArray
    val mapping = new ColumnMapping(variables)
    val dat = data.view.map(l => new DenseData(l.toArray, mapping)).toIndexedSeq
    new DataFrame(dat, mapping)
  }

  def apply(data: Iterable[Seq[Double]]): DataFrame[DenseData] = {
    val n = data.head.size
    val names = (0 until n).map(i => "V" + i).toSeq
    apply(data, names)
  }


  def apply(data: Iterable[Map[Any, Double]])(implicit d: DummyImplicit): DataFrame[SparseData] = {
    val variableMapping = data.view.flatMap(_.keys.map(_.toString))
      .toSet.map((s: String) => (s, new NumericVariable(s))).toMap
    val mapping = new ColumnMapping(variableMapping.values.toList)
    val n = mapping.columns.size
    val dat = data.view
      .map(d => new SparseData(d.map { case (k, v) => (mapping(variableMapping(k.toString)), v) }, n, mapping))
      .toIndexedSeq
    new DataFrame(dat, mapping)
  }

}
