import scala.collection.mutable
import Aliases._
/**
 * User: mick
 * Date: 08.05.13
 * Time: 17:52
 */
trait LocalFeatureSet {
  // list of lighted up features
  def g(tag_2: Tag, tag_1: Tag, sentence: Array[Word], i: Int, t: Tag): List[Int]
  // find features idx with num of encounter
  def g(sentence: TaggedSentence): FVector

  // add feature in string representation with idx i
  def add(params: Array[String], i: Int)

  // scan tagged sentence for features
  def findFeatures(sentence: TaggedSentence)

  // feature idx and string representation
  def toMap: Map[Int, Array[String]]

  def shift(num: Int)

  def size: Int
}

trait KeyGen {
  def apply(tag_2: Tag, tag_1: Tag, sentence: Array[Word], i: Int, t: Tag): String
  def apply(params: Array[String]): String
  def apply(params: String*): String
  def fromKey(key: String): Array[String]
}

object TagKeyGen extends KeyGen {
  def key(word: String, tag: String) = word + ":" + tag

  def apply(tag_2: Tag, tag_1: Tag, sentence: Array[Word], i: Int, t: Tag) =
    if (i >= sentence.length) key("__NULL__", t)
    else key(sentence(i), t)

  def apply(params: Array[String]) =
    key(params(0), params(1))

  def apply(params: String*) =
    key(params(0), params(1))

  def fromKey(key: String) = key.split(":")
}

object TrigramKeyGen extends KeyGen {
  def key(tag_2: String, tag_1: String, tag: String) = tag_2 + ":" + tag_1 + ":" + tag

  def apply(tag_2: Tag, tag_1: Tag, sentence: Array[Word], i: Int, t: Tag) =
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
  def g(tag_2: Tag, tag_1: Tag, sentence: Array[Word], i: Int, t: Tag): List[Int] =
    keyToIdx.get(keyGen(tag_2, tag_1, sentence, i, t)).toList

  def g(sentence: TaggedSentence): FVector = {
    (for (i <- 2 to (sentence.length - 1))
      yield g(sentence(i - 2).tag, sentence(i - 1).tag, sentence.map(_.word), i, sentence(i).tag)).
    foldLeft(Map.empty[Int, Int]) {(acc, fs) =>
      fs.foldLeft(acc) {(facc, fidx) =>
        val num = facc.getOrElse(fidx, 0)
        acc + (fidx -> (num + 1))
      }
    }
  }

  // add feature in string representation with idx i
  def add(params: Array[String], i: Int) {
    val key = keyGen(params)
    keyToIdx.put(keyGen(params), i)
  }

  // feature idx and string representation
  def toMap: Map[Int, Array[String]] = keyToIdx.map(kv => (kv._2, keyGen.fromKey(kv._1))).toMap

  def shift(num: Int) {
    val newMap = keyToIdx.toList.zipWithIndex.map(t => (t._1._1 -> (t._1._2 + num + t._2)))
    keyToIdx.clear()
    keyToIdx ++= newMap
  }

  def size = keyToIdx.size
}

class TagFeatures extends MapLikeFeatures {
  val keyGen = TagKeyGen
  def findFeatures(sentence: TaggedSentence) {
    sentence.foreach(wordTag => add(Array(wordTag.word, wordTag.tag), 0))
  }
}

class TrigramsFeatures extends MapLikeFeatures {
  val keyGen = TrigramKeyGen
  def findFeatures(sentence: TaggedSentence) {
    sentence.sliding(3).foreach(slice => add(Array(slice(0).tag, slice(1).tag, slice(2).tag), 0))
  }
}

