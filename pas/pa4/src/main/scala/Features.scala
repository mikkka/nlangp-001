import scala.collection.mutable
/**
 * User: mick
 * Date: 08.05.13
 * Time: 17:52
 */
trait LocalFeatureSet {
  // list of lighted up features
  def g(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): List[Int]

  // add feature in string representation with idx i
  def add(params: Array[String], i: Int)

  // scan tagged sentence for features
  def findFeatures(sentence: Array[(String,String)])

  // feature idx and string representation
  def toMap: Map[Int, Array[String]]
}

trait KeyGen {
  def apply(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): String
  def apply(params: Array[String]): String
  def apply(params: String*): String
  def fromKey(key: String): Array[String]
}

object TagKeyGen extends KeyGen {
  def key(word: String, tag: String) = word + ":" + tag

  def apply(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String) =
    key(sentence(i), tag_1)

  def apply(params: Array[String]) =
    key(params(0), params(1))

  def apply(params: String*) =
    key(params(0), params(1))

  def fromKey(key: String) = key.split(":")
}

object TrigramKeyGen extends KeyGen {
  def key(tag_2: String, tag_1: String, tag: String) = tag_2 + ":" + tag_1 + ":" + tag

  def apply(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String) =
    key(tag_2, tag_1, t)

  def apply(params: Array[String]) =
    key(params(0), params(1), params(2))

  def apply(params: String*) =
    key(params(0), params(1), params(2))

  def fromKey(key: String) = key.split(":")
}

abstract class MapLikeFeatures extends LocalFeatureSet {
  val keyToIdx = mutable.Map.empty[String, Int]
  val keyGen: KeyGen

  // list of lighted up features
  def g(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): List[Int] =
    keyToIdx.get(keyGen(tag_2, tag_2, sentence, i, t)).toList

  // add feature in string representation with idx i
  def add(params: Array[String], i: Int) {
    keyToIdx.put(keyGen(params), i)
  }

  // feature idx and string representation
  def toMap: Map[Int, Array[String]] = keyToIdx.map(kv => (kv._2, keyGen.fromKey(kv._1))).toMap
}

class TagFeatures extends MapLikeFeatures {
  val keyGen = TagKeyGen
  def findFeatures(sentence: Array[(String,String)]) {
    sentence.foreach(wordTag => add(Array(wordTag._1, wordTag._2), 0))
  }
}

class TrigramsFeatures extends MapLikeFeatures {
  val keyGen = TrigramKeyGen
  def findFeatures(sentence: Array[(String,String)]) {
    sentence.sliding(3).foreach(slice => add(Array(slice(0)._2, slice(1)._2, slice(2)._2), 0))
  }
}

