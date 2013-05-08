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

class TagFeatures extends LocalFeatureSet {
  val wordTagToIdxMap = mutable.Map.empty[String, Int]

  def key(word: String, tag: String) = word + ":" + tag

  def g(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): List[Int] =
    wordTagToIdxMap.get(key(sentence(i), tag_1)).toList

  //word, tag
  def add(params: Array[String], i: Int) {
    add(params(0), params(1), i)
  }

  def add(word: String, tag: String, i: Int) {
    wordTagToIdxMap.put(key(word, tag), i)
  }

  def findFeatures(sentence: Array[(String,String)]) {
    sentence.foreach(wordTag => add(wordTag._1, wordTag._2, 0))
  }

  // just reverse the map
  def toMap: Map[Int, Array[String]] = wordTagToIdxMap.map(kv => (kv._2, kv._1.split(":"))).toMap
}

class TrigramsFeatures extends LocalFeatureSet {
  val tag1tag2ToIdxMap = mutable.Map.empty[String, Int]

  def key(tag_2: String, tag_1: String, tag: String) = tag_2 + ":" + tag_1 + ":" + tag

  def g(tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): List[Int] =
    tag1tag2ToIdxMap.get(key(tag_2, tag_1, t)).toList

  //tag_2, tag_1, tag
  def add(params: Array[String], i: Int) {
    add(params(0), params(1), params(2), i)
  }

  def add(tag_2: String, tag_1: String, tag: String, i: Int) {
    tag1tag2ToIdxMap.put(key(tag_2, tag_1, tag), i)
  }

  def findFeatures(sentence: Array[(String,String)]) {
    sentence.sliding(3).foreach(slice => add(slice(0)._2, slice(1)._2, slice(2)._2, 0))
  }

  // just reverse the map
  def toMap: Map[Int, Array[String]] =  tag1tag2ToIdxMap.map(kv => (kv._2, kv._1.split(":"))).toMap
}

