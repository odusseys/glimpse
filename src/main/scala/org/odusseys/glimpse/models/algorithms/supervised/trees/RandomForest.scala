package org.odusseys.glimpse.models.algorithms.supervised.trees

import org.odusseys.glimpse.data.{DataFrame, Data}
import org.odusseys.glimpse.models.formulas.Formula

import scala.util.Random

/**
 * Created by umizrahi on 16/03/2016.
 */
class RandomForest(formula: Formula,
                   nTrees: Int,
                   depth: Int,
                   minWeightsPerNode: Int = 0,
                   nVariablesSampled: Int = 0,
                   seed: Random = new Random()) {

  def train[DataType <: Data](data: DataFrame[DataType]) = {
    val reader = formula.decodeFor(data)
    val nf = if (nVariablesSampled <= 0) {
      math.sqrt(reader.factorVariables.size + reader.numericVariables.size).toInt
    } else {
      nVariablesSampled
    }
    val trees = (1 to nTrees) map { _ =>
      new DecisionTree(formula, depth, minWeightsPerNode, nf, seed).train(data)
    }
    new RandomForestModel(trees.toList)
  }

}

class RandomForestModel[DataType <: Data](val trees: List[DecisionTreeModel[DataType]]) {
  def predict(t: DataType) = {
    trees.map(_.predict(t)).sum / trees.size
  }
}
