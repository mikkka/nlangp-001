/**
 * User: mick
 * Date: 24.04.13
 * Time: 22:21
 */
class Corpus(enCorpusPath: String, esCorpusPath: String) {
  val NULL_WORLD = "__NULL__"

  val eSentences = FileIO.linesFromFile(enCorpusPath).map(line => Vector(NULL_WORLD) ++ line.split(" ").toVector).toVector
  val fSentences = FileIO.linesFromFile(esCorpusPath).map(line => line.split(" ").toVector).toVector
  lazy val zipped = eSentences zip fSentences
}
