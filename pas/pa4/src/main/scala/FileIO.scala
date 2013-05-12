import java.io.PrintWriter

/**
 * User: mick
 * Date: 12.05.13
 * Time: 13:58
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
