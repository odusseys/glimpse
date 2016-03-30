package org.odusseys.glimpse.models.formulas

import org.odusseys.glimpse.data.{DataFrame, Data}

/**
 * Created by umizrahi on 22/03/2016.
 */
class Formula(s: String, fromNames: Boolean) {

  import Formula._

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

object Formula {

  implicit def toFormula(s: String): Formula = new Formula(s, true)

  val formulaSeparator = "="
  val columnSeparator = ','
  val exceptions = "^ *\\* *\\- *(.*)".r
  val wildcard = "*"
  val opening = '('
  val closing = ')'

  def parseExceptions(s: String) = {
    exceptions.findFirstMatchIn(s).map(_.group(1).split(columnSeparator).map(_.trim).toList)
  }

  val wildcardExpression = "^ *\\*".r

  def getMember(s: String) = {
    import FeatureProcessingSyntax._
    val wildcard = wildcardExpression.findFirstMatchIn(s).isDefined
    val exceptions = parseExceptions(s).getOrElse(List())
    val terms = if (wildcard) List() else s.split(columnSeparator).toList
    new FormulaMember(
      wildcard,
      terms.map(s => parseSyntaxTree(s)),
      exceptions)
  }

}

class FormulaMember(val wildcard: Boolean,
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

/*todo : This should be refactored to classes and should be generic (double/int) so that
* we can create factor -> numeric or numeric -> factor feature mappers*/
object FormulaFunctions {

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

