package org.odusseys.glimpse.data

import scala.collection.mutable

/**
 * Created by umizrahi on 08/03/2016.
 */
class FactorProcessor {
  private val mapping = new mutable.HashMap[String, Int]()
  private val inverseMapping = new mutable.HashMap[Int, String]()

  def process(level: String) = {
    if (!mapping.contains(level)) {
      val i = mapping.size
      mapping.put(level, i)
      inverseMapping.put(i, level)
    }
  }

  def encodedLevel(level: String) = mapping(level)

  def decodedLevel(level: Int) = inverseMapping(level)
}
