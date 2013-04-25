import java.io.PrintWriter

/**
 * User: mick
 * Date: 25.04.13
 * Time: 17:20
 */
object FileIO {
  def withPrintWriter[A](file: String)(f: PrintWriter => A): Option[A] = {
    Some(new PrintWriter(file)).map{p =>
      val res = f(p)
      p.close()
      res
    }
  }

  def linesFromFile(file: String) = scala.io.Source.fromFile(file).getLines()
}
