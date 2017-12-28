package in.vigneshwaran.skiing

import scala.io.Source
import scala.runtime.ScalaRunTime._

object Main extends App {

  val iter = Source.fromFile(args(0)).getLines()
  val Seq(rows, cols) = iter.next().split(" ").map(_.toInt).toSeq
  val map: Map = Array.ofDim[Int](rows, cols)


  iter.zipWithIndex.foreach { case (line, i) =>
    line.split(" ").zipWithIndex.foreach { case (elev, j) =>
      map(i)(j) = elev.toInt
    }
  }

  val (length, drop) = new SkiRouteFinder().findLongestAndSteepestDive(map)
  println(s"\n Result: $length -> $drop")
}

