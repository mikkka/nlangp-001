type Sentence = scala.collection.mutable.ListBuffer[(String,String)]

//lowercase word?
// word -> tag
val words = scala.io.Source.fromFile(args(0)).getLines().map( _.trim().split(" ") match {
  case Array(word, tag) => (word -> tag)
  case _ => ("" -> "STOP")
}).toList

def countWords(words: List[(String, String)]) = words.filter(_._2 != "STOP").groupBy(p => p).map(p => (p._1 -> p._2.size))

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
val wordCounts_1 = countWords(words_1)
//all words count
val county = wordCounts_1.groupBy(_._1._2).map(p => (p._1 -> p._2.foldLeft(0)(_ + _._2)))

//println(wordCounts_1)
//println(county)

def argmaxyexy(word: String) = {
  val igene = wordCounts_1.get((word -> "I-GENE"))
  val o =  wordCounts_1.get((word -> "O"))

  val igene_r = wordCounts_1.get(("_RARE_" -> "I-GENE"))
  val o_r =  wordCounts_1.get(("_RARE_" -> "O"))

  //println(word + ": " + "I-GENE " + igene.getOrElse(0) + "/" + igene_r.getOrElse(0) + "; O " + o.getOrElse(0) + "/" + o_r.getOrElse(0))

  if(!igene.isEmpty && !o.isEmpty)
    if(igene.get * 1.0 / county("I-GENE") > o.get * 1.0 / county("O")) "I-GENE" else "O"
  else if(!igene.isEmpty)
    "I-GENE"
    //if(igene.get * 1.0 / county("I-GENE") > o_r.get * 1.0/ county("O")) "I-GENE" else "O"
  else if(!o.isEmpty)
    "O"
    //if(igene_r.get * 1.0 / county("I-GENE") > o.get * 1.0 / county("O")) "I-GENE" else "O"
  else 
    if(igene_r.get * 1.0 / county("I-GENE") > o_r.get * 1.0 / county("O")) "I-GENE" else "O"
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
