package org.odusseys.glimpse.models.algorithms.supervised.trees


import org.odusseys.glimpse.data.{Data, DataFrame}
import org.odusseys.glimpse.models.formulas.{FormulaReader, Formula}
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.util.Random


/**
 * Created by umizrahi on 14/03/2016.
 */
class DecisionTreeFeatures(formula: Formula,
                   depth: Int = 5,
                   minWeightsPerNode: Double = 1,
                   featuresPerSplit: Int = 0,
                   seed: Random = new Random()) {

  import DecisionTree._

  def sampleVariables[DataType <: Data](numericVariables: Array[DataType => Double],
                                        factorVariables: Array[DataType => Int]): (Array[DataType => Double], Array[DataType => Int]) = {
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

  }

  class DTNode[DataType <: Data](val id: Int,
                                 response: DataType => Double,
                                 weight: DataType => Double,
                                 numericVariables: Array[DataType => Double],
                                 factorVariables: Array[DataType => Int]) {

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

