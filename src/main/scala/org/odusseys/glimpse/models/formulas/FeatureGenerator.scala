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

  require(!(leftMember.wildcard && rightMember.wildcard),
    "Cannot have both response and variable wildcards !")

  require((leftMember.exceptions.isEmpty || leftMember.wildcard) &&
    (rightMember.exceptions.isEmpty || rightMember.wildcard),
    "Cannot have exceptions without a wildcard !")


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

class Member(val wildcard: Boolean,
             val terms: List[FeatureProcessingSyntax],
             val exceptions: List[String]) {

  private def extractRawFromSyntax(syntax: FeatureProcessingSyntax): List[String] = {
    syntax match {
      case Raw(s) => List(s)
      case Unary(p, arg) => extractRawFromSyntax(arg)
      case Binary(p, first, sec) => extractRawFromSyntax(first) ++ extractRawFromSyntax(sec)
    }
  }

  def extractRawTerms = terms.flatMap(extractRawFromSyntax)

}

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
    val exceptions = parseExceptions(s).getOrElse(List())
    val terms = if (wildcard) List() else s.split(columnSeparator).toList
    new Member(
      wildcard,
      terms.map(s => parseSyntaxTree(s)),
      exceptions)
  }

}

trait RawFeatureHandler {

  def process(token: String): Option[Feature[_]]

  def eligibleTokens: List[String]
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

  override def eligibleTokens = data.mapping.columns.map(_.name).toList

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

  override def eligibleTokens = (0 until data.mapping.size) map (_.toString) toList

}

class FeatureProcessor(rawFeatureHandler: RawFeatureHandler) {

  def getFeature(syntax: FeatureProcessingSyntax) = {
    process(syntax) match {
      case Some(f) => f
      case None => throw new IllegalArgumentException("Could not parse formula token with syntax " + syntax) //not great
    }
  }

  private def process(syntax: FeatureProcessingSyntax): Option[Feature[_]] = {
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


  private val processor = new FeatureProcessor(rawFeatureHandler)
  private val allTokens = rawFeatureHandler.eligibleTokens
  private val responses = (if (!formula.leftMember.wildcard) {
    formula.leftMember.terms
  } else {
    val rawRight = formula.rightMember.extractRawTerms
    (allTokens.toSet -- (rawRight ++ formula.leftMember.exceptions)).map(s => Raw(s)).toList
  }).map(processor.getFeature)

  private val variables = (if (!formula.rightMember.wildcard) {
    formula.rightMember.terms
  } else {
    val rawLeft = formula.leftMember.extractRawTerms
    (allTokens.toSet -- (rawLeft ++ formula.rightMember.exceptions)).map(s => Raw(s)).toList
  }).map(processor.getFeature)

  private val _numericVariables = variables.collect{case n: NumericFeature => n}.toArray
  private val _factorVariables = variables.collect{case n: FactorFeature => n}.toArray
  private val _numericResponses = responses.collect{case n: NumericFeature => n}.toArray
  private val _factorResponses = responses.collect{case n: FactorFeature => n}.toArray


  override def numericVariables: Array[NumericFeature] = _numericVariables

  override def numericResponses: Array[NumericFeature] = _numericResponses

  override def factorResponses: Array[FactorFeature] = _factorResponses

  override def factorVariables: Array[FactorFeature] = _factorVariables
}
