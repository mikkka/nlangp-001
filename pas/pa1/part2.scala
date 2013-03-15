import scala.collection.mutable.Map
//import scala.collection.mutable.ListBuffer

//type Sentence = scala.collection.mutable.ListBuffer[(String,String)]
//lowercase word?
// word -> tag

//CONSTS!!!
val h1 = 0.4
val h2 = 0.4
val h3 = 0.2
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

def wordsReplace(words: List[(String, String)], rareWords: Set[String], replacement: String) = words.map(p => 
  if(rareWords.contains(p._1)) (replacement -> p._2)
  else p
).toList

val rareWords = words.map(_._1).groupBy(x => x).filter(_._2.size < 5).keySet
val words_1 = wordsReplace(words, rareWords, "_RARE_")

val wordsCounts = countWords(words_1) // unigrams!
val tagCounts = words_1.groupBy(_._2).map(p => (p._1 -> p._2.size))
val sentences = toSentences(words)

val parSentences = sentences.par
val grams1 = parSentences.map(_.map(_._2)).flatMap(p => p).groupBy(p => p).map(p => (p._1 -> p._2.size)).seq
val totalGrams1 = grams1.foldLeft(0){_ + _._2}

val grams2 = parSentences.map(_.map(_._2).sliding(2)).flatMap(p => p).collect{case Vector(x,y) => (x,y)}.groupBy(p => p).map(p => (p._1 -> p._2.size)).seq
val grams3 = parSentences.map(_.map(_._2).sliding(3)).flatMap(p => p).collect{case Vector(x,y,z) => (x,y,z)}.groupBy(p => p).map(p => (p._1 -> p._2.size)).seq
/*
println("grams1")
println(grams1)

println("grams2")
println(grams2)

println("grams3")
println(grams3)
*/
def exy(word: String, tag: String) = {
  if(wordsCounts.contains(word)) wordsCounts(word).getOrElse(tag, 0) * 1.0 / tagCounts(tag)
  else wordsCounts("_RARE_")(tag) * 1.0 / tagCounts(tag)
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
  println("tagging sentence : " + xs)

  var pi = scala.collection.mutable.Map((0,"*","*") -> (1.0,"*"))

  def S(k: Int) = 
    if (k < 1) List("*")
    else List("I-GENE", "O")

  for(k <- 1 to n) {
    println(k)
    val xk = xs(k - 1)._1
    for(
      u <- S(k - 1);
      v <- S(k)
    ) {
      println(u + " " + v + " " + xk)
      pi +=(
      (k, u, v) -> 
      (for(w <- S(k - 2)) yield {
        val piw = pi(k-1,w,u)._1 * q((w,u,v)) * exy(xk,v)

        println(w + " " + piw)
        (piw, w)
      }).maxBy(_._1))
      println("")
    }
    println("")
  }

  println(pi)

  val tail = (for(
    u <- S(n - 1);
    v <- S(n)
  ) yield {
    ((pi(n, u, v)._1 * q(u, v, "STOP")) -> (u, v))
  }).maxBy(_._1)

  println(tail)

  sentence
}
/*
println("train data loaded")
//test q print
println("example q")

println(q(("*", "*", "O")))
println(q(("*", "O", "O")))
println(q(("O", "O", "O")))
println(q(("O", "O", "STOP")))
println(q(("*", "O", "STOP")))

println(q(("*", "*", "I-GENE")))
println(q(("*", "I-GENE", "I-GENE")))
println(q(("I-GENE", "I-GENE", "I-GENE")))
println(q(("I-GENE", "I-GENE", "STOP")))
println(q(("*", "I-GENE", "STOP")))

println(q(("O", "I-GENE", "I-GENE")))
println(q(("O", "O", "I-GENE")))
println(q(("I-GENE", "I-GENE", "O")))
println(q(("I-GENE", "O", "O")))

println(q(("O", "I-GENE", "O")))
println(q(("I-GENE", "O", "I-GENE")))

println(q(("O", "I-GENE", "STOP")))
println(q(("I-GENE", "O", "STOP")))
*/

val wordsForTag = scala.io.Source.fromFile(args(1)).getLines().map{line => 
  val str = line.trim()
  if(!str.isEmpty) (str -> "")
  else ("" -> "STOP")
}.toList

val sentencesForTag = toSentences(wordsForTag).par
//val taggedSentences = sentencesForTag.par.map(tagSentence)
//taggedSentences.foreach(println(_))
println(tagSentence(sentencesForTag(0)))
//println(tagSentence(sentencesForTag(1)))



