package org.odusseys.glimpse.data

/**
 * Created by umizrahi on 04/03/2016.
 */
class DataFrame[DataType <: Data](val data: Seq[DataType], val mapping: ColumnMapping) {

  def print() = {
    println(mapping.toSeq.sortBy(_._1).map(_._2.name).mkString("\t"))
    data.foreach { l => println((0 until l.size).map(l.apply).mkString("\t")) }
  }

  def nColumns = mapping.size

  def columns = mapping.columns

}

object DataFrame {

  implicit def toSeq[T <: Data](data: DataFrame[T]): Seq[T] = data.data

  def apply(data: Iterable[Seq[Double]], columnNames: Seq[String]): DataFrame[DenseData] = {
    val n = columnNames.size
    require(data.forall(_.size == n), "All sub-collections in the provided collection must have the same size !")
    val variables = (0 until n) map { i => new NumericVariable(columnNames(i)) } toArray
    val mapping = new ColumnMapping(variables)
    val dat = data.view.map(l => new DenseData(l.toArray)).toSeq
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
      .map(d => new SparseData(d.map { case (k, v) => (mapping(variableMapping(k.toString)), v) }, n))
      .toSeq
    new DataFrame(dat, mapping)
  }

}
