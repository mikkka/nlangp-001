/**
 * User: mick
 * Date: 12.05.13
 * Time: 13:59
 */
import Aliases._
import collection.parallel.mutable

object FeaturesIO {
  def readFeatures(fileName: String): (V, Set[LocalFeatureSet]) = {
    val trigramFeatures = new TrigramsFeatures
    val tagFeatures = new TagFeatures
    var v = Vector[Double]()

    var counter = 0
    FileIO.linesFromFile(fileName).foreach{l: String =>
      val partsAndParam = l.split(" ")
      val parts = partsAndParam(0).split(":")
      parts(0) match {
        case "TAG" => {
          val fixedParts = parts.drop(1).dropRight(1)
          val tag = parts.last
          tagFeatures.add(Array(fixedParts.mkString(":"), tag), counter)
        }
        case "TRIGRAM" => trigramFeatures.add(parts.drop(1), counter)
        case _ => throw new IllegalStateException("bad input : " + l)
      }
      v = v :+ partsAndParam.last.toDouble
      counter = counter+1
    }

    (v, Set(trigramFeatures, tagFeatures))
  }
}
