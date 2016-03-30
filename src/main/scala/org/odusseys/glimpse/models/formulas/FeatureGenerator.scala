package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{FactorColumn, NumericColumn, DataFrame, Data}

/**
 * Created by umizrahi on 18/03/2016.
 */

trait FeatureGenerator[DataType <: Data] {
  def numericVariables: Array[NumericFeature]

  def numericResponses: Array[NumericFeature]

  def factorVariables: Array[FactorFeature]

  def factorResponses: Array[FactorFeature]

  val signature = (numericResponses.length + factorResponses.length, numericVariables.length + factorVariables.length)

}

class ColumnGenerator[DataType <: Data](data: DataFrame[DataType],
                                        formula: Formula,
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

  private val _numericVariables = variables.collect { case n: NumericFeature => n }.toArray
  private val _factorVariables = variables.collect { case n: FactorFeature => n }.toArray
  private val _numericResponses = responses.collect { case n: NumericFeature => n }.toArray
  private val _factorResponses = responses.collect { case n: FactorFeature => n }.toArray

  override def numericVariables: Array[NumericFeature] = _numericVariables

  override def numericResponses: Array[NumericFeature] = _numericResponses

  override def factorResponses: Array[FactorFeature] = _factorResponses

  override def factorVariables: Array[FactorFeature] = _factorVariables
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
      case Some(n: NumericColumn) => Some(new NumericColumnFeature(n, index))
      case Some(f: FactorColumn) => Some(new FactorColumnFeature(f, index))
      case _ => None
    }
  }

  override def eligibleTokens = (0 until data.mapping.size) map (_.toString) toList

}

class ColumnNameRawFeatureHandler[DataType <: Data](data: DataFrame[DataType]) extends RawFeatureHandler {
  override def process(token: String): Option[Feature[_]] = {
    val mapping = data.mapping
    val columnCandidate = mapping.get(token).map(i => mapping(i)) match {
      case Some(n: NumericColumn) => Some(new NumericColumnFeature(n, mapping(n)))
      case Some(f: FactorColumn) => Some(new FactorColumnFeature(f, mapping(f)))
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

  override def eligibleTokens = data.mapping.columns.map(_.name).toList

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
          case Some(num: NumericFeature) => Some(new MappedNumericFeature(num, FormulaFunctions(token), token))
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

