/**
 * User: mick
 * Date: 12.05.13
 * Time: 14:07
 */
import Aliases._

object SentenceIO {
  def readSentences(fileName: String): Vector[Sentence] = {
    val wordsForTag = FileIO.linesFromFile(fileName).map{line: String =>
      val str = line.trim()
      if(!str.isEmpty) WordTag(str, "")
      else WordTag("", "STOP")
    }.toList

    val sentencesForTag = toSentences(wordsForTag)
    sentencesForTag.map(sentence => sentence.drop(2).dropRight(1).map(_.word).toArray)
  }


  def toSentences(words: List[WordTag]) = {
    def iter(acc: Vector[Vector[WordTag]], tail: List[WordTag]) : Vector[Vector[WordTag]] = {
      if(tail.isEmpty) acc
      else {
        val (sentence, rest) = tail.span(_.tag != "STOP")
        val newAcc = acc :+ (Vector(WordTag("" , "*"), WordTag("" , "*")) ++ sentence :+ WordTag("", "STOP"))
        if(rest.isEmpty) newAcc
        else iter(newAcc, rest.tail)
      }
    }
    iter(Vector.empty, words)
  }

  def writeTagged(fileName: String, taggedSentences: Vector[TaggedSentence]) {
    FileIO.withPrintWriter(fileName) {pw =>
      taggedSentences.foreach{s =>
        s.foreach(wt => pw.println(wt.word + " " + wt.tag))
        pw.println("")
      }
    }
  }
}
