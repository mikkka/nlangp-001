/**
 * User: mick
 * Date: 24.04.13
 * Time: 22:21
 */
class Corpus(enCorpusPath: String, esCorpusPath: String) {
  val en = scala.io.Source.fromFile(enCorpusPath).getLines().map(line => line.split(" ").toVector).toVector
  val es = scala.io.Source.fromFile(esCorpusPath).getLines().map(line => line.split(" ").toVector).toVector
}
