/**
 * User: mick
 * Date: 12.05.13
 * Time: 13:59
 */
class FeaturesIO {
  def readFeatures(fileName: String) = {
    val trigramFeatures = new TrigramsFeatures
    val tagFeatures = new TagFeatures

    var counter = 0
    FileIO.linesFromFile(fileName).foreach{l: String =>
      val parts = l.split(":")
      parts(0) match {
        case "TAG" => tagFeatures.add(parts.drop(1), counter)
        case "TRIGRAM" => tagFeatures.add(parts.drop(1), counter)
      }
      counter = counter+1
    }
  }
}
