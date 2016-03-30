package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.models.formulas.{ConstantFeature, FactorFeature, Formula, NumericFeature}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.util.Random
import scala.util.parsing.json.JSONObject


/**
 * Created by umizrahi on 14/03/2016.
 */
class DecisionTree(formula: Formula,
                   depth: Int = 5,
                   minWeightsPerNode: Double = 1,
                   featuresPerSplit: Int = 0,
                   seed: Random = new Random()) {

  import DecisionTree._

  def sampleVariables[DataType <: Data](numericVariables: Array[NumericFeature],
                                        factorVariables: Array[FactorFeature]): (Array[NumericFeature], Array[FactorFeature]) = {
    if (featuresPerSplit <= 0) {
      return (numericVariables, factorVariables)
    }
    val (num, fac) = seed.shuffle(0 until (numericVariables.length + factorVariables.length) toList)
      .take(featuresPerSplit)
      .partition(_ < numericVariables.length)
    (num.map(numericVariables(_)).toArray,
      fac.map(i => i - numericVariables.length).map(factorVariables(_)).toArray)
  }

  def train[DataType <: Data](data: DataFrame[DataType]) = {

    val reader = formula.decodeFor(data)
    require(Set(1, 2).contains(reader.signature._1), "Formula should have 1 or 2 responses (2 if using weights)")
    val weights = if (reader.signature._1 == 2) {
      reader.numericResponses(1)
    } else {
      new ConstantFeature(1.0)
    }
    val response = reader.numericResponses(0)
    val numericVariables = reader.numericVariables
    val factorVariables = reader.factorVariables
    val nodeMap = new mutable.HashMap[Int, DTNode[DataType]]
    val root = new DTNode[DataType](0, response, weights, numericVariables, factorVariables)
    nodeMap.put(0, root)
    var toSplit = Map(root -> data.toIndexedSeq)
    for (i <- 1 to depth) {
      val newSplits = new mutable.HashMap[DTNode[DataType], IndexedSeq[DataType]]
      val splitResults = toSplit.flatMap { case (n, dat) => n.split(dat) }
      splitResults.foreach { case (left, right, leftData, rightData) =>
        nodeMap.put(left.id, left)
        nodeMap.put(right.id, right)
        newSplits.put(left, leftData)
        newSplits.put(right, rightData)
      }
      toSplit = newSplits.toMap
      if (i == depth) {
        toSplit.foreach(u => u._1.computeStats(u._2)) //label the leaves !
      }
    }

    def makeNode(i: Int) = {
      val n = nodeMap(i)
      if (nodeMap.contains(2 * i + 1)) {
        new SplitNode(n.id, n.split.get match {
          case NumericDTSplit(loss, variable, split) =>
            new NumericSplit[DataType](split, numericVariables(variable))
          case FactorDTSplit(loss, variable, levels) =>
            new FactorSplit[DataType](
              levels,
              i => data.mapping(variable).decode(i),
              factorVariables(variable)
            )
        })
      } else {
        new Leaf(i, n.label)
      }
    }

    new DecisionTreeModel(nodeMap.keys.map(i => (i, makeNode(i))).toMap)

  }

  /*
    def train[DataType <: Data](reader: FormulaReader[DataType], data: Seq[DataType]) = {

      require(Set(1, 2).contains(reader.signature._1), "Formula should have 1 or 2 responses (2 if using weights)")
      val weights = if (reader.signature._1 == 2) {
        reader.numericResponses(1)
      } else {
        (d: DataType) => 1.0
      }
      val response = reader.numericResponses(0)
      val numericVariables = reader.numericVariables
      val factorVariables = reader.factorVariables
      val numericVariableNames = reader.numericVariableNames
      val factorVariableNames = reader.factorVariableNames
      val nodeMap = new mutable.HashMap[Int, DTNode[DataType]]
      val root = new DTNode[DataType](0, response, weights, numericVariables, factorVariables)
      nodeMap.put(0, root)
      var toSplit = Map(root -> data.toIndexedSeq)
      for (i <- 1 to depth) {
        val newSplits = new mutable.HashMap[DTNode[DataType], IndexedSeq[DataType]]
        val splitResults = toSplit.flatMap { case (n, dat) => n.split(dat) }
        splitResults.foreach { case (left, right, leftData, rightData) =>
          nodeMap.put(left.id, left)
          nodeMap.put(right.id, right)
          newSplits.put(left, leftData)
          newSplits.put(right, rightData)
        }
        toSplit = newSplits.toMap
        if (i == depth) {
          toSplit.foreach(u => u._1.computeStats(u._2)) //label the leaves !
        }
      }

      def makeNode(i: Int) = {
        val n = nodeMap(i)
        if (nodeMap.contains(2 * i + 1)) {
          new SplitNode(n.id, n.split.get match {
            case NumericDTSplit(loss, variable, split) =>
              new NumericSplit(split, numericVariables(variable), numericVariableNames(variable))
            case FactorDTSplit(loss, variable, levels) =>
              new FactorSplit(
                levels,
                i => data.mapping(variable).decode(i),
                factorVariables(variable),
                factorVariableNames(variable)
              )
          })
        } else {
          new Leaf(i, n.label)
        }
      }

      new DecisionTreeModel(nodeMap.keys.map(i => (i, makeNode(i))).toMap)
    }*/

  class DTNode[DataType <: Data](val id: Int,
                                 response: NumericFeature,
                                 weight: NumericFeature,
                                 numericVariables: Array[NumericFeature],
                                 factorVariables: Array[FactorFeature]) {

    var split: Option[DTSplit] = None
    var totalWeights: Double = Double.NaN
    var label: Double = Double.NaN
    val (sampledNumerics, sampledFactors) = sampleVariables(numericVariables, factorVariables)

    def goesLeft(t: DataType) = {
      split.get match {
        case NumericDTSplit(loss, variable, s) => sampledNumerics(variable)(t) <= s
        case FactorDTSplit(loss, variable, levels) => levels.contains(sampledFactors(variable)(t))
      }
    }

    def computeStats(data: Seq[DataType]) = {
      totalWeights = data.map(weight).sum
      label = data.view.map(l => response(l) * weight(l)).sum / totalWeights
    }

    class SplitComputer[T] {

      val totalLabels = label * totalWeights
      var leftLabels = 0.0
      var leftWeights = 0.0
      var bestLoss = 0.0
      var best: Option[T] = None

      def process(t: T, labels: Double, weights: Double): Unit = {
        if (weights <= 0.0) return
        leftLabels = leftLabels + labels
        leftWeights = leftWeights + weights
        if (leftWeights < minWeightsPerNode || (totalWeights - leftWeights) < minWeightsPerNode) return
        val l = loss(leftLabels, totalLabels - leftLabels, leftWeights, totalWeights - leftWeights)
        if (l < bestLoss) {
          bestLoss = l
          best = Some(t)
        }
      }

    }

    def computeBestNumericSplit(data: Seq[DataType]): Option[DTSplit] = {
      def computeNumericSplit(index: Int, variable: DataType => Double) = {
        val computer = new SplitComputer[Double]
        data.toStream.map(l => (variable(l), response(l), weight(l)))
          .sortBy(_._1)
          .dropRight(1)
          .foreach { case (v, l, w) => computer.process(v, l, w) }
        computer.best.map(u => NumericDTSplit(computer.bestLoss, index, u))
      }
      sampledNumerics.indices.flatMap(i => computeNumericSplit(i, sampledNumerics(i))) match {
        case Seq() => None
        case results => Some(results.minBy(_.loss))
      }
    }

    def computeBestFactorSplit(data: Seq[DataType]): Option[DTSplit] = {
      def computeFactorSplit(index: Int, variable: DataType => Int) = {
        val stats = new mutable.HashMap[Int, Stat]
        data.foreach { l =>
          val i = variable(l)
          val lab = response(l)
          val w = weight(l)
          if (stats.contains(i)) {
            stats(i).increment(lab, w)
          } else {
            stats.put(i, new Stat(lab, w))
          }
        }
        val computer = new SplitComputer[Int]
        val sortedStats = stats.toList.view
          .sortBy { case (i, st) => st.labelSum / st.weightSum }
          .dropRight(1).force
        sortedStats.foreach { case (i, st) => computer.process(i, st.labelSum, st.weightSum) }
        val levels = {
          val (before, after) = sortedStats.map(_._1).span(i => i != computer.best.get)
          before.toSet ++ Set(after.head)
        }
        computer.best.map(u => FactorDTSplit(computer.bestLoss, index, levels))
      }
      sampledFactors.indices.flatMap(i => computeFactorSplit(i, sampledFactors(i))) match {
        case Seq() => None
        case results => Some(results.minBy(_.loss))
      }
    }


    def split(data: IndexedSeq[DataType]):
    Option[(
      DTNode[DataType],
        DTNode[DataType],
        IndexedSeq[DataType],
        IndexedSeq[DataType]
      )] = {
      computeStats(data)
      val numericSplit = computeBestNumericSplit(data)
      val factorSplit = computeBestFactorSplit(data)
      val bestSplit = (numericSplit, factorSplit) match {
        case (None, None) => None
        case (Some(ns), None) => Some(ns)
        case (None, Some(fs)) => Some(fs)
        case (Some(ns), Some(fs)) => Some(if (ns.loss < fs.loss) ns else fs)
      }
      split = bestSplit
      split.map { s =>
        val (leftData, rightData) = data.partition(goesLeft)
        (
          new DTNode[DataType](2 * id + 1, response, weight, numericVariables, factorVariables),
          new DTNode[DataType](2 * id + 2, response, weight, numericVariables, factorVariables),
          leftData,
          rightData
          )
      }
    }
  }

}

object DecisionTree {

  private[trees] class Stat(var labelSum: Double, var weightSum: Double) {
    def increment(lab: Double, wt: Double) = {
      labelSum = labelSum + lab
      weightSum = weightSum + wt
    }
  }

  private[trees] abstract class DTSplit(val loss: Double, val variable: Int)

  private[trees] case class NumericDTSplit(override val loss: Double, override val variable: Int, split: Double)
    extends DTSplit(loss, variable)

  private[trees] case class FactorDTSplit(override val loss: Double, override val variable: Int, levels: Set[Int])
    extends DTSplit(loss, variable)


  private[trees] def loss(leftLabels: Double, rightLabels: Double, leftWeights: Double, rightWeights: Double) = {
    val p = (leftLabels + rightLabels) / (leftWeights + rightWeights)
    val u1 = leftLabels / leftWeights - p
    val u2 = rightLabels / rightWeights - p
    -leftWeights * u1 * u1 - rightWeights * u2 * u2
  }

}


sealed abstract class Node[+DataType <: Data](val id: Int, val isLeaf: Boolean) {
  def toJSON: JSONObject
}

case class SplitNode[DataType <: Data](override val id: Int, split: Split[DataType]) extends Node[DataType](id, false) {
  def toJSON = {
    new JSONObject(Map("isLeaf" -> false, "id" -> id, "split" -> split.toJSON))
  }
}

case class Leaf(override val id: Int, label: Double) extends Node[Nothing](id, true) {
  def toJSON = {
    new JSONObject(Map("isLeaf" -> true, "id" -> id, "label" -> label))
  }
}

class DecisionTreeModel[DataType <: Data](nodes: Map[Int, Node[DataType]]) {

  def nNodes = nodes.size

  def predict(t: DataType): Double = {
    var n = nodes(0)
    while (!n.isLeaf) {
      n = if (n.asInstanceOf[SplitNode[DataType]].split.goesLeft(t)) {
        nodes(2 * n.id + 1)
      } else {
        nodes(2 * n.id + 2)
      }
    }
    n.asInstanceOf[Leaf].label
  }

  override def toString = {
    val s = new StringBuilder()
    nodes.toList.sortBy(_._1).foreach { case (i, n) =>
      for (i <- 1 to (31 - Integer.numberOfLeadingZeros(i))) {
        s.append(" ")
      }
      n match {
        case Leaf(id, label) => s.append("leaf : " + label)
        case SplitNode(id, split) => s.append("node : ")
      }
      s.append("\n")
    }
    s.toString()
  }

  private[trees] def relabelLeaves(relabeling: Int => Double) = {
    new DecisionTreeModel(nodes.map { case (i, n) => n match {
      case SplitNode(id, split: Split[DataType]) => (i, n)
      case Leaf(id, l) => (i, Leaf(i, relabeling(i)))
    }
    })
  }

}

