package org.odusseys.glimpse.io

import breeze.linalg.DenseVector
import org.odusseys.glimpse.data._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by umizrahi on 08/03/2016.
 */
object Import {

  def fromCSV(path: String, header: Boolean = true, sep: String = "\t") = {
    val it = Source.fromFile(path).getLines()
    val firstLine = it.next().split(sep)
    val names = if (header) firstLine.map(_.trim) else (1 to firstLine.length).map { i => "V" + i }.toArray
    val n = names.length

    val processor = new Processor(n)
    if (!header) processor.process(firstLine)
    processor.process(it.map(_.split(sep)))
    val numeric = processor.result
    val variables = numeric.indices.map { i =>
      if (numeric(i))
        new NumericVariable(names(i))
      else
        new FactorVariable(names(i))
    }.toArray
    val mapping = new ColumnMapping(variables)

    def processLine(s: String) = {
      val ar = s.split(sep).map(_.trim)
      val dat = ar.indices.map { i =>
        if (numeric(i))
          ar(i).toDouble
        else variables(i).asInstanceOf[FactorVariable].process(ar(i)).toDouble
      }.toArray
      new DenseData(new DenseVector(dat), mapping)
    }

    val buf = new ArrayBuffer[DenseData]
    Source.fromFile(path).getLines().drop(if (header) 1 else 0).foreach(s => buf.append(processLine(s)))
    new DataFrame(buf.toSeq, mapping)
  }


  private class Processor(n: Int) {
    val num = "[-+]?([0-9]+\\.?[0-9]*|[0-9]*\\.?[0-9]+)([eE][-+]?[0-9]+)?".r

    def isNum(s: String) = {
      val t = num.findFirstIn(s).isDefined
      t
    }

    val result = Array.fill(n)(true)

    def process(arr: Array[String]): Unit = {
      require(arr.length == n, "All lines in the file should have the same number of elements.")
      arr.indices.foreach(i => result(i) = result(i) && isNum(arr(i)))
    }

    def process(it: Iterator[Array[String]]): Unit = it.foreach(process)

  }

  def fromLibSVM(path: String) = {

    def decode(s: String): Map[Any, Double] = {
      val ar = s.split(" ")
      Map("Y".asInstanceOf[Any] -> ar(0).toDouble) ++ ar.drop(1).map { t =>
        val components = t.split(":")
        "V" + components(0) -> components(1).toDouble
      }
    }

    val it = Source.fromFile(path).getLines().map(decode).toIterable
    DataFrame(it.toIterable)
  }

}
