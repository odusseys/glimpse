package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{DataFrame, Data}

/**
 * Created by umizrahi on 18/03/2016.
 */

class NewFormula(s: String) {

  import NewFormula._

  val (leftMember, rightMember) = {
    val membs = s.split(formulaSeparator)
    require(membs.length == 2, s"Formula should have 2 members separated by a $formulaSeparator")
    (getMember(membs(0)), getMember(membs(1)))
  }

}

class Member(wildcard: Boolean, terms: Option[List[String]], exceptions: Option[List[String]])

object FormulaFunction {

  def apply(s: String) = mapper.getOrElse(s,
    throw new NoSuchElementException(s"No function with name $s available for formulae.")
  )

  private val mapper = Map(
    "exp" -> math.exp _,
    "log" -> math.log _,
    "sin" -> math.sin _,
    "cos" -> math.cos _,
    "sqrt" -> math.sqrt _,
    "floor" -> math.floor _,
    "ceil" -> math.ceil _,
    "abs" -> ((t: Double) => math.abs(t))
  )

  val names = mapper.keySet

  def isFunction(s: String) = mapper.contains(s)

  val regex = names.mkString("|").r

}

object FormulaOperators {

  case class Operator(op: (Double, Double) => Double, precedence: Int)

  private val mapper = Map[String, Operator](
    """+""" -> Operator(_ + _, 1),
    """*""" -> Operator(_ * _, 2),
    """/""" -> Operator(_ / _, 3),
    """-""" -> Operator(_ - _, 1)
  )

  def precedence(s: String) = mapper(s).precedence

  def isOperator(s: String) = names.contains(s)

  val names = mapper.keySet
  // val regex = expressions.mkString("|").r
}

object NewFormula {

  implicit def toFormula(s: String): NewFormula = new NewFormula(s)

  val formulaSeparator = "="
  val columnSeparator = ','
  val exceptions = "^ *\\* *\\- *(.*)".r
  val wildcard = "*"
  val opening = '('
  val closing = ')'

  def parseExceptions(s: String) = {
    exceptions.findFirstMatchIn(s).map(_.group(1).split(columnSeparator).toList)
  }

  val wildcardExpression = "^ *\\*".r

  def getMember(s: String) = {
    val wildcard = wildcardExpression.findFirstMatchIn(s).isDefined
    val exceptions = parseExceptions(s)
    val terms = if (wildcard) None else Some(s.split(columnSeparator).toList)
    new Member(wildcard, terms, exceptions)
  }

}

trait RawFeatureHandler {

  def process[T](token: String): Option[Feature[T]]
}


class FeatureProcessor(rawFeatureHandler: RawFeatureHandler) {
  def process(syntax: FeatureProcessingSyntax): Option[Feature[_]] = {
    syntax match {
      case Raw(token) => rawFeatureHandler.process(token)
      case Unary(token, argument) =>
        process(argument) match {
          case Some(num: NumericFeature) => Some(new MappedNumericFeature(num, FormulaFunction(token), token))
          case _ => None
        }
      case Binary(token, first, second) =>
        val firstFeature = process(first)
        val secondFeature = process(second)
        (firstFeature, secondFeature) match {
          case (Some(firstNum: NumericFeature), Some(secondNum: NumericFeature)) =>
          case _ => None
        }
    }
  }
}

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
