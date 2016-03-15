package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{Data, DataFrame, Variable}
import org.odusseys.glimpse.models.formulas.{FormulaReader, Formula}

import scala.collection.mutable


/**
 * Created by umizrahi on 14/03/2016.
 */
class DecisionTree(formula: Formula, depth: Int = 5, minWeightsPerNode: Double = 1) {

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
    val nodeMap = new mutable.HashMap[Int, DTNode[DataType]]
    val root = new DTNode[DataType](0)
    nodeMap.put(0, root)
    var toSplit = Map(root -> data.toSeq)

    for (i <- 1 to depth) {
      val newSplits = new mutable.HashMap[DTNode[DataType], Seq[DataType]]
      val splitResults = toSplit.flatMap { case (n, dat) => n.split(data, response, weights, numericVariables, factorVariables) }
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
        new SplitNode(n.id, n.split.get)
      } else {
        new Leaf[DataType](i, n.label)
      }
    }

    new DecisionTreeModel(nodeMap.keys.map(i => (i, makeNode(i))).toMap)

  }

  class DTNode[DataType <: Data](val id: Int) {

    var split: Option[Split[DataType]] = None
    var totalWeights: Double = Double.NaN
    var label: Double = Double.NaN

    def computeStats(data: Seq[DataType],
                     response: DataType => Double,
                     weight: DataType => Double) = {
      totalWeights = data.map(weight).sum
      label = data.view.map(l => response(l) * weight(l)).sum / totalWeights
    }

    class Stat(var labelSum: Double, var weightSum: Double) {
      def increment(lab: Double, wt: Double) = {
        labelSum = labelSum + lab
        weightSum = weightSum + wt
      }
    }

    def loss(leftLabels: Double, rightLabels: Double, leftWeights: Double, rightWeights: Double) = {
      val p = (leftLabels + rightLabels) / (leftWeights + rightWeights)
      val u1 = leftLabels / leftWeights - p
      val u2 = rightLabels / rightWeights - p
      -leftWeights * u1 * u1 - rightWeights * u2 * u2
    }

    abstract class DTSplit(loss: Double, variable: Int)

    case class NumericDTSplit(loss: Double, variable: Int, split: Double) extends DTSplit(loss, variable)

    case class FactorDTSplit(loss: Double, variable: Int, levels: Set[Int]) extends DTSplit(loss, variable)

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

    def computeBestNumericSplit(data: Seq[DataType],
                                response: DataType => Double,
                                weight: DataType => Double,
                                numericVariables: Array[DataType => Double]): Option[DTSplit] = {
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

    def computeBestFactorSplit(data: Seq[DataType],
                               response: DataType => Double,
                               weight: DataType => Double,
                               factorVariables: Array[DataType => Int]): Option[DTSplit] = {


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
        def getLevels = {
          val levels = new mutable.HashSet[Int]
          val it = sortedStats.iterator.map(_._1)
          var a: Int = -1
          do {
            a = it.next()
            levels.add(a)
          } while (a != computer.best.get && it.hasNext)
          levels.toSet
        }
        computer.best.map(u => FactorDTSplit(computer.bestLoss, index, getLevels))
      }

    }


    def split(data: Seq[DataType],
              response: DataType => Double,
              weight: DataType => Double,
              numericVariables: Array[DataType => Double],
              factorVariables: Array[DataType => Int]): Option[(DTNode[DataType], DTNode[DataType], Seq[DataType], Seq[DataType])] = {
      computeStats(data, response, weight)
      val numericSplit = computeBestNumericSplit(data, response, weight, numericVariables)
      val factorSplit = computeBestFactorSplit(data, response, weight, factorVariables)
      val bestSplit = (numericSplit, factorSplit) match {
        case (None, None) => None
        case (Some(ns), None) => Some(ns)
        case (None, Some(fs)) => Some(fs)
        case (Some(ns), Some(fs)) => Some(if (ns.loss < fs.loss) ns else fs)
      }
      split = bestSplit.map(_.split)
      split.map { s =>
        val (leftData, rightData) = data.partition(s.goesLeft)
        (new DTNode[DataType](2 * id + 1), new DTNode[DataType](2 * id + 2), leftData, rightData)
      }
    }
  }

  case class SplitResult[DataType <: Data](loss: Double, split: Split[DataType])


}


sealed abstract class Node[DataType <: Data](val id: Int, val isLeaf: Boolean)

class SplitNode[DataType <: Data](override val id: Int, val split: Split[DataType]) extends Node[DataType](id, false)

class Leaf[DataType <: Data](override val id: Int, label: Double) extends Node[DataType](id, true)

class DecisionTreeModel[DataType <: Data](nodes: Map[Int, Node[DataType]])

