package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{Data, DataFrame, Variable}
import org.odusseys.glimpse.models.formulas.{FormulaReader, Formula}

import scala.annotation.tailrec
import scala.collection.mutable


/**
 * Created by umizrahi on 14/03/2016.
 */
class DecisionTree(formula: Formula, depth: Int = 5, minWeightsPerNode: Double = 1) {

  import DecisionTree._

  def train[DataType <: Data](data: DataFrame[DataType]) = {

    val reader = formula.decodeFor(data)
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
    var toSplit = Map(root -> data.toSeq)

    for (i <- 1 to depth) {
      val newSplits = new mutable.HashMap[DTNode[DataType], Seq[DataType]]
      val splitResults = toSplit.flatMap { case (n, dat) => n.split(data) }
      splitResults.foreach { case (left, right, leftData, rightData) =>
        nodeMap.put(left.id, left)
        nodeMap.put(right.id, right)
        newSplits.put(left, leftData)
        newSplits.put(right, rightData)
      }
      toSplit = newSplits.toMap
    }

    def makeNode(i: Int) = {
      val n = nodeMap(i)
      if (nodeMap.contains(2 * i + 1)) {
        new SplitNode(n.id, n.split.get match {
          case NumericDTSplit(loss, variable, split) =>
            new NumericSplit(split, numericVariables(variable), numericVariableNames(variable))
          case FactorDTSplit(loss, variable, levels) =>
            new FactorSplit(levels, factorVariables(variable), factorVariableNames(variable))
        })
      } else {
        new Leaf[DataType](i, n.label)
      }
    }

    new DecisionTreeModel(nodeMap.keys.map(i => (i, makeNode(i))).toMap)

  }

  class DTNode[DataType <: Data](val id: Int,
                                 response: DataType => Double,
                                 weight: DataType => Double,
                                 numericVariables: Array[DataType => Double],
                                 factorVariables: Array[DataType => Int]) {

    var split: Option[DTSplit] = None
    var totalWeights: Double = Double.NaN
    var label: Double = Double.NaN

    def goesLeft(t: DataType) = {
      split.get match {
        case NumericDTSplit(loss, variable, s) => numericVariables(variable)(t) <= s
        case FactorDTSplit(loss, variable, levels) => levels.contains(factorVariables(variable)(t))
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
        data.view.map(l => (variable(l), response(l), weight(l)))
          .sortBy(_._1)
          .dropRight(1)
          .foreach { case (v, l, w) => computer.process(v, l, w) }
        computer.best.map(u => NumericDTSplit(computer.bestLoss, index, u))
      }
      numericVariables.indices.flatMap(i => computeNumericSplit(i, numericVariables(i))) match {
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
      factorVariables.indices.flatMap(i => computeFactorSplit(i, factorVariables(i))) match {
        case Seq() => None
        case results => Some(results.minBy(_.loss))
      }
    }


    def split(data: Seq[DataType]): Option[(DTNode[DataType], DTNode[DataType], Seq[DataType], Seq[DataType])] = {
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


sealed abstract class Node[DataType <: Data](val id: Int, val isLeaf: Boolean)

case class SplitNode[DataType <: Data](override val id: Int, split: Split[DataType]) extends Node[DataType](id, false)

case class Leaf[DataType <: Data](override val id: Int, label: Double) extends Node[DataType](id, true)

class DecisionTreeModel[DataType <: Data](nodes: Map[Int, Node[DataType]]) {

  def predict(t: DataType): Double = {
    var n = nodes(0)
    while (!n.isLeaf) {
      n = if (n.asInstanceOf[SplitNode[DataType]].split.goesLeft(t)) {
        nodes(2 * n.id + 1)
      } else {
        nodes(2 * n.id + 2)
      }
    }
    n.asInstanceOf[Leaf[DataType]].label
  }

}

