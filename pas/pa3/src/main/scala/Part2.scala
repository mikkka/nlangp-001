/**
 * User: mick
 * Date: 28.04.13
 * Time: 22:01
 */
object Part2 extends App {
  val corp = new Corpus(args(0), args(1))
  val params1 = Params1(args(2))
  val params2 = Params2(corp, params1)

  Params2.save(params2, args(3))
}
