/**
 * User: mick
 * Date: 24.04.13
 * Time: 22:21
 */
class Corpus(enCorpusPath: String, esCorpusPath: String) {
  val en = FileIO.linesFromFile(enCorpusPath).map(line => line.split(" ").toVector).toVector
  val es = FileIO.linesFromFile(esCorpusPath).map(line => line.split(" ").toVector).toVector
}
