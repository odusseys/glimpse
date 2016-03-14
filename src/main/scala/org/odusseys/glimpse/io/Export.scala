package org.odusseys.glimpse.io

import java.io.PrintWriter

import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.models.formulas.Formula

/**
 * Created by umizrahi on 10/03/2016.
 */
object Export {
  def toCSV[T <: Data](data: DataFrame[T], path: String, sep: String = "\t", header: Boolean = true) = {
    val pw = new PrintWriter(path, "UTF-8")
    val columns = data.columns.toArray
    try {
      if (header) {
        pw.println(columns.map(_.name).mkString(sep))
      }
      data.foreach { l =>
        pw.println((0 until l.size) map { i => columns(i).decode(l(i)) } mkString sep)
      }
    } finally {
      if (pw != null) pw.close()
    }

  }

  def toLibSVM[T <: Data](data: DataFrame[T], formula: Formula, path: String) = {
    val reader = formula.decodeFor(data)
    require(reader.signature._1 == 1, "data must have a single response ! ")
    val sparseVariables = reader.sparseNumericVariables
    val response = reader.numericResponses(0)
    val pw = new PrintWriter(path, "UTF-8")

    def decode(d: T) = {
      val s = new StringBuilder
      s.append(response(d))
      sparseVariables(d).foreach { case (i, v) => s.append(" " + i + ":" + v) }
    }

    try {
      data.foreach(d => pw.println(decode(d)))
    } finally {
      if (pw != null) pw.close()
    }
  }

}
