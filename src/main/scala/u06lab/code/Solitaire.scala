package u06lab.code

import u06lab.code.Solitaire.w

import java.awt.event.ItemListener
import scala.collection.immutable.HashSet

object Solitaire extends App:
  type Coordinate = (Int, Int)
  type Solution = Iterable[Coordinate]
  type IterableFactory = Solution => Iterable[Solution]
  val w = 7
  val h = 5
  val step = 3

  given IterableFactory = List(_).view

  val result = placeMarks(w, h)
  for (item <-result)println(render(item.toSeq, w, h) + "\n")
  println(result.size)

  def placeMarks(w: Int, h: Int)(using factory: IterableFactory): Iterable[Solution] =

    val movements = Set((-step, 0), (step, 0), (0, -step), (0, step), (-2, -2), (2, 2), (-2, 2), (2, -2))
    val coordinates = for
      i <- 0 until w
      j <- 0 until h
    yield
      (i, j)

    def searchSolutions(c: Coordinate)(l: IndexedSeq[Coordinate]) : Iterable[Solution] = l.size match
      case 1 =>
        if (for i <- movements yield (c._1 + i._1, c._2 + i._2)) contains l.head then
          factory(l)
        else
          List()
      case _ =>
        for
          i <- movements
          n = (c._1 + i._1, c._2 + i._2)
          if l contains n
          soluzion <- searchSolutions(n)(l.filter(_ != n))
        yield
          soluzion.toSeq :+ n

    val init = (w/2, h/2)
    for
      solution <- searchSolutions(init)(coordinates.filter(_ != init))
    yield
      solution.toSeq :+ init

//    val movements = Set(-step, step, -((w + 2 * step) * step), (w + 2 * step) * step, -((w + 2 * step) * 2 - 2), (w + 2 * step) * 2 - 2, -((w + 2 * step) * 2 + 2), (w + 2 * step) * 2 + 2)
//    val coordinates = for
//      i <- 0 until h
//      base = 3 + (step + i) * (2 * step + w)
//      j <- base until base + w
//    yield
//      j
//
//    def searchSolutions(c: Int)(l: IndexedSeq[Int]) : Iterable[Solution] = l.size match
//      case 1 =>
//        if (for i <- movements yield c + i) contains l.head then
//          factory(Set((l.head % (w + 2 * step) - step, l.head / (w + 2 * step) - step)))
//        else
//          List()
//      case _ =>
//        for
//          i <- movements
//          n = c + i
//          if l contains n
//          soluzion <- searchSolutions(n)(l.filter(_ != n))
//        yield
//          soluzion.toSeq :+ (n % (w + 2 * step) - step, n / (w + 2 * step) - step)
//
//    val init = (((w + 2 * step) * (h + 2 * step) ) - 1) / 2
//    for
//      solution <- searchSolutions(init)(coordinates.filter(_ != init))
//    yield
//      solution.toSeq :+ (init % (w + 2 * step) - step, init / (w + 2 * step) - step)


  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")