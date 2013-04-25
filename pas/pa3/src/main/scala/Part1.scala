/**
 * User: mick
 * Date: 24.04.13
 * Time: 22:20
 */
object Part1 extends App {
  val corp = new Corpus(args(0), args(1))
  val params1 = Params1(corp)

  Params1.save(params1, args(2))
}
