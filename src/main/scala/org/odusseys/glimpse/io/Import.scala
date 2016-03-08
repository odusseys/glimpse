package org.odusseys.glimpse.io

import org.odusseys.glimpse.data.{NumericVariable, Data, DataFrame}

import scala.io.Source

/**
 * Created by umizrahi on 08/03/2016.
 */
object Import {
  def fromCSV(path: String, header: Boolean = true, sep: String = "\t"): DataFrame[Data] = {
    val it = Source.fromFile(path).getLines()
    val firstLine = it.next().split(sep)
    val names = if (header) firstLine.map(_.trim) else (1 to firstLine.length).map { i => "V" + i }.toArray
    val n = names.length
    val processor = new Processor(n)
    processor.process(firstLine)
    processor.process(it.map(_.split(sep)))
    val numeric = processor.result
    val variables = numeric.indices.map(num => if(num) new NumericVariable(names(i)))
  }

  private class Processor(n: Int) {
    val num = "[0-9]*\\.?[0-9]+".r

    def isNum(s: String) = num.findFirstIn(s).isDefined

    val result = Array.fill(n)(false)

    def process(arr: Array[String]) = {
      require(arr.length == n,"All lines in the file should have the same number of elements.")
      arr.indices.foreach(i => result(i) = result(i) && isNum(arr(i)))
    }

    def process(it: Iterator[Array[String]]) = it.foreach(process)

  }

}
