package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{FactorVariable, NumericVariable, DataFrame, Data}

/**
 * Created by umizrahi on 18/03/2016.
 */

class NewFormula(s: String, fromNames: Boolean) {

  import NewFormula._

  val (leftMember, rightMember) = {
    val membs = s.split(formulaSeparator)
    require(membs.length == 2, s"Formula should have 2 members separated by a $formulaSeparator")
    (getMember(membs(0)), getMember(membs(1)))
  }

  def decodeFor[DataType <: Data](data: DataFrame[DataType]): FeatureGenerator[DataType] =
    new ColumnGenerator(
      data,
      this,
      if (fromNames)
        new ColumnNameRawFeatureHandler(data)
      else
        new ColumnIndexRawFeatureHandler(data)
    )

}

class Member(wildcard: Boolean,
             terms: Option[List[FeatureProcessingSyntax]],
             exceptions: Option[List[FeatureProcessingSyntax]])

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

}

object FormulaOperators {

  def apply(s: String) = mapper.getOrElse(s,
    throw new NoSuchElementException(s"No function with name $s available for formulae.")
  )

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
}

object NewFormula {

  implicit def toFormula(s: String): NewFormula = new NewFormula(s, true)

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
    import FeatureProcessingSyntax._
    val wildcard = wildcardExpression.findFirstMatchIn(s).isDefined
    val exceptions = parseExceptions(s)
    val terms = if (wildcard) None else Some(s.split(columnSeparator).toList)
    new Member(
      wildcard,
      terms.map(_.map(s => parseSyntaxTree(s))),
      exceptions.map(_.map(s => parseSyntaxTree(s))))
  }

}

trait RawFeatureHandler {

  def process(token: String): Option[Feature[_]]
}

class ColumnIndexRawFeatureHandler[DataType <: Data](data: DataFrame[DataType]) extends RawFeatureHandler {
  override def process(token: String): Option[Feature[_]] = {
    val index = try {
      token.toInt
    } catch {
      case e: NumberFormatException => return None
    }
    data.mapping.get(index) match {
      case Some(n: NumericVariable) => Some(new NumericColumnFeature(n, index))
      case Some(f: FactorVariable) => Some(new FactorColumnFeature(f, index))
      case _ => None
    }
  }

}

class ColumnNameRawFeatureHandler[DataType <: Data](data: DataFrame[DataType]) extends RawFeatureHandler {
  override def process(token: String): Option[Feature[_]] = {
    val mapping = data.mapping
    val columnCandidate = mapping.get(token).map(i => mapping(i)) match {
      case Some(n: NumericVariable) => Some(new NumericColumnFeature(n, mapping(n)))
      case Some(f: FactorVariable) => Some(new FactorColumnFeature(f, mapping(f)))
      case _ => None
    }
    columnCandidate.orElse(
      try {
        Some(new ConstantFeature(token.toDouble))
      } catch {
        case e: NumberFormatException => None
      }
    )
  }

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
          case (Some(firstNum: NumericFeature), Some(secondNum: NumericFeature)) => Some(
            new BinaryMappedNumericFeatures(firstNum, secondNum, FormulaOperators(token).op, token, true)
          )
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

class ColumnGenerator[DataType <: Data](data: DataFrame[DataType],
                                        formula: NewFormula,
                                        rawFeatureHandler: RawFeatureHandler)
  extends FeatureGenerator[DataType] {


  override def numericVariables: Array[NumericFeature] = ???

  override def numericResponses: Array[NumericFeature] = ???

  override def factorResponses: Array[FactorFeature] = ???

  override def factorVariables: Array[FactorFeature] = ???
}
