import scala.collection.mutable.Map

//CONSTS!!!
val h1 = 0.33
val h2 = 0.34
val h3 = 0.33

val hasNumberPattern = ".*\\d.*".r.pattern
//

def mem[A,B](f: A => B) = new Function[A,B] {
  private var cache: Map[A,B] = scala.collection.mutable.Map()
  def apply(v: A): B = cache getOrElseUpdate(v, f(v))
}

val words = scala.io.Source.fromFile(args(0)).getLines().map( _.trim().split(" ") match {
  case Array(word, tag) => (word -> tag)
  case _ => ("" -> "STOP")
}).toList

def countWords(words: List[(String, String)]) = words.filter(_._2 != "STOP").
  groupBy(_._1).map(p => (p._1 -> p._2.groupBy(_._2).map(p => (p._1 -> p._2.size))))

def toSentences(words: List[(String,String)]) = {
  def iter(acc: Vector[Vector[(String, String)]], tail: List[(String, String)]) : Vector[Vector[(String, String)]] = {
    if(tail.isEmpty) acc
    else {
      val (sentence, rest) = tail.span(_._2 != "STOP")
      val newAcc = acc :+ (Vector("" -> "*", "" -> "*") ++ sentence :+ ("" -> "STOP"))
      if(rest.isEmpty) newAcc
      else iter(newAcc, rest.tail)
    }
  }
  iter(Vector.empty, words)
}

def wordsReplace(words: List[(String, String)], rareWords: Set[String]) = words.map(p => 
  if(rareWords.contains(p._1)) (wordClass(p._1) -> p._2)
  else p
).toList

def wordClass(word: String) = 
  if(word.forall(_.isUpper)) "_RARE_UC_"
  else if(word.exists(_.isDigit)) "_RARE_NUM_"
  else if(word.last.isUpper) "_RARE_LUC_"
  else "_RARE_"

val rareWords = words.map(_._1).groupBy(x => x).filter(_._2.size < 5).keySet
val words_1 = wordsReplace(words, rareWords)

val wordsCounts = countWords(words_1) // unigrams!
val tagCounts = words_1.groupBy(_._2).map(p => (p._1 -> p._2.size))
val sentences = toSentences(words)

val parSentences = sentences.par
val grams1 = parSentences.map(_.map(_._2)).flatMap(p => p).groupBy(p => p).map(p => (p._1 -> p._2.size)).seq
val totalGrams1 = grams1.foldLeft(0){_ + _._2}

val grams2 = parSentences.map(_.map(_._2).sliding(2)).flatMap(p => p).collect{case Vector(x,y) => (x,y)}.groupBy(p => p).map(p => (p._1 -> p._2.size)).seq
val grams3 = parSentences.map(_.map(_._2).sliding(3)).flatMap(p => p).collect{case Vector(x,y,z) => (x,y,z)}.groupBy(p => p).map(p => (p._1 -> p._2.size)).seq

def exy(word: String, tag: String) = {
  if(wordsCounts.contains(word)) wordsCounts(word).getOrElse(tag, 0) * 1.0 / tagCounts(tag)
  else wordsCounts(wordClass(word))(tag) * 1.0 / tagCounts(tag)
}

def argmaxyexy(word: String) = {
  if(exy(word, "I-GENE") > exy(word, "O")) "I-GENE"
  else "O"
}

def q(t: (String, String, String)) = {
  h1 * grams3.getOrElse(t, 0) / grams2((t._1, t._2)) + h2 * grams2((t._2, t._3)) / grams1(t._2) + h3 * grams1(t._3) / totalGrams1
}

def memq = mem(q)

def tagSentence(sentence: Vector[(String, String)]) = {
  val xs = sentence.drop(2).dropRight(1)
  val n = xs.length

  var pi = scala.collection.mutable.Map((0,"*","*") -> (1.0,"*"))

  def S(k: Int) = 
    if (k < 1) List("*")
    else List("I-GENE", "O")

  for(k <- 1 to n) {
    val xk = xs(k - 1)._1
    for(
      u <- S(k - 1);
      v <- S(k)
    ) {
      pi +=(
      (k, u, v) -> 
      (for(w <- S(k - 2)) yield {
        val piw = pi(k-1,w,u)._1 * memq((w,u,v)) * exy(xk,v)
        (piw, w)
      }).maxBy(_._1))
    }
  }

  //backtracking
  val tagsTail = (for(
    u <- S(n - 1);
    v <- S(n)
  ) yield {
    ((pi(n, u, v)._1 * memq(u, v, "STOP")) -> (u, v))
  }).maxBy(_._1)._2
  val tags = new Array[String](n)
  tags(n-1) = tagsTail._2
  tags(n-2) = tagsTail._1

  for(k <- (n - 2) to 1 by -1) {
    val i = k - 1
    tags(i) = pi(k + 2, tags(i + 1), tags(i + 2))._2
  } 

  xs.zipWithIndex.map(p => (p._1._1, tags(p._2)))
}

val wordsForTag = scala.io.Source.fromFile(args(1)).getLines().map{line => 
  val str = line.trim()
  if(!str.isEmpty) (str -> "")
  else ("" -> "STOP")
}.toList

val sentencesForTag = toSentences(wordsForTag)
val taggedSentences = sentencesForTag.map(tagSentence)
taggedSentences.foreach{s =>
  s.foreach(wt => println(wt._1 + " " + wt._2))
  println("")
}


