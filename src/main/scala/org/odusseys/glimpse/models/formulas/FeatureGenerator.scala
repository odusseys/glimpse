package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{DataFrame, Data}

import scala.StringBuilder
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by umizrahi on 18/03/2016.
 */

class NewFormula(s: String) {

  import NewFormula._

  val (leftMember, rightMember) = {
    val membs = s.split(formulaSeparator)
    require(membs.length == 2, s"Formula should have 2 members separated by a $formulaSeparator")
    (getTerms(membs(0)), getTerms(membs(1)))
  }

  class Member(wildcard: Boolean, terms: List[String], exceptions: List[String])


}

object FormulaFunction {

  def apply(s: String) = mapper.get(s).orElse(
    throw new NoSuchElementException(s"No function with name $s available for formulae.")
  )

  private val mapper = Map(
    "exp" -> math.exp _,
    "log" -> math.log _,
    "sin" -> math.sin _,
    "cos" -> math.cos _,
    "sqrt" -> math.sqrt _
  )

}

object NewFormula {

  implicit def toFormula(s: String): NewFormula = new NewFormula(s)

  val formulaSeparator = "="
  val columnSeparator = '+'
  val exceptions = "^ *\\* *\\- *(.*)".r
  val wildcard = "*"
  val factor = "factor\\((.+)\\)".r
  val opening = '('
  val closing = ')'

  def factorize(s: String) = {
    factor.findFirstMatchIn(s).map(_.group(1))
  }

  def parseExceptions(s: String) = {
    exceptions.findFirstMatchIn(s).map(_.group(1))
  }

  def getTerms(s: String) = {
    var nPar = 0
    val sb = new StringBuilder
    val buff = new ArrayBuffer[String]
    s.foreach {
      case ' ' =>
      case `opening` =>
        if (nPar != 0) {
          sb.append(opening)
        }
        nPar = nPar + 1
      case `closing` =>
        nPar = nPar - 1
        if (nPar != 0) {
          sb.append(closing)
        }
      case `columnSeparator` =>
        if (nPar == 0) {
          buff.append(sb.toString())
          sb.clear()
        } else {
          sb.append(columnSeparator)
        }
      case c => sb.append(c)
    }
    buff.append(sb.toString())
    buff.toList
  }

  def split(s: String) = {
    val sp = s.split(formulaSeparator)
    sp.length match {
      case 2 => Some(sp(0), sp(1))
      case _ => None
    }

  }

  def main(args: Array[String]) {
    val a = "a + b + c"
    val b = "(a + b + c)"
    val c = "(a + b ) + c"
    println(getTerms(a))
    println(getTerms(b))
    println(getTerms(c))
  }

}

trait FeatureProcessor

trait FeatureGenerator[DataType <: Data] {
  def numericVariables: Array[NumericFeature]

  def numericResponses: Array[NumericFeature]

  def factorVariables: Array[FactorFeature]

  def factorResponses: Array[FactorFeature]

}

class ColumnNameGenerator[DataType <: Data](data: DataFrame[DataType], f: Formula)
  extends FeatureGenerator[DataType] {
  override def numericVariables: Array[NumericFeature] = ???

  override def numericResponses: Array[NumericFeature] = ???

  override def factorResponses: Array[FactorFeature] = ???

  override def factorVariables: Array[FactorFeature] = ???
}
