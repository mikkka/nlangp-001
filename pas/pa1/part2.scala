type Sentence = scala.collection.mutable.ListBuffer[(String,String)]

//lowercase word?
// word -> tag
val words = scala.io.Source.fromFile(args(0)).getLines().map( _.trim().split(" ") match {
  case Array(word, tag) => (word -> tag)
  case _ => ("" -> "STOP")
}).toList

def countWords(words: List[(String, String)]) = words.filter(_._2 != "STOP").
  groupBy(_._1).map(p => (p._1 -> p._2.groupBy(_._2).map(p => (p._1 -> p._2.size))))

// _RARE_
// неверно! rare определяется по всему количеству слов в тексте, а не по подгруппам!
val rareWords = words.map(_._1).groupBy(x => x).filter(_._2.size < 5).keySet

//println(rareWords)

val words_1 = words.map(p => 
  if(rareWords.contains(p._1))
    ("_RARE_" -> p._2)
  else 
    p
).toList
// group words into (word -> tag) -> count map
val wordsCounts = countWords(words_1)
//all words count
val tagCounts = words_1.groupBy(_._2).map(p => (p._1 -> p._2.size))

//println(wordCounts_1)
//println(county)
def exy(word: String, tag: String) = {
  if(wordsCounts.contains(word)) wordsCounts(word).getOrElse(tag, 0) * 1.0 / tagCounts(tag)
  else wordsCounts("_RARE_")(tag) * 1.0 / tagCounts(tag)
}

def argmaxyexy(word: String) = {
  if(exy(word, "I-GENE") > exy(word, "O")) "I-GENE"
  else "O"
}

//end of train
val tagged = scala.io.Source.fromFile(args(1)).getLines().map{line => 
  val word = line.trim()
  if(!word.isEmpty) Some((word -> argmaxyexy(word)))
  else None
}.toList

tagged.foreach(_ match {
  case Some((word, tag)) => println(word + " " + tag)
  case _ => println()
})
