import org.scalatest.{FlatSpec, MustMatchers}

import scala.collection.mutable
import scala.util.Random

class MazeTest extends FlatSpec with MustMatchers {

  behavior of "Maze"

  "generateMaze" must "return a new Maze when requested" in {
    MazeGenerator.generateMaze(3, 4) mustBe an[Maze]
  }

  it must "return a Maze with specified width and height" in {
    val m = MazeGenerator.generateMaze(3, 4)
    m.width must equal(3)
    m.height must equal(4)
  }

  "maze" should "return a set of neighbour cells for a given cell" in {
    val m = MazeGenerator.generateMaze(4, 4)
    m(2, 3) mustBe an[mutable.Set[_]]
  }

  it must "have at least one route out of each cell" in {
    val m = MazeGenerator.generateMaze(4, 4)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      m(x, y).size must be > 0
    }
  }

  it must "have a matching route in for each route out of a cell" in {
    val m = MazeGenerator.generateMaze(4, 4)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      val me = Cell(x, y)
      m(x, y).foreach(n => {
        m(n.x, n.y) must contain(me)
      })
    }
  }

  // now I can't tell that it's actually created a maze ...

  "initNeighbours" must "return 4 neighbours for an internal cell" in {
    val n = Neighbours(6, 6)
    n(3, 4) must contain(Cell(3, 5))
    n(3, 4) must contain(Cell(2, 4))
    n(3, 4) must contain(Cell(4, 4))
    n(3, 4) must contain(Cell(3, 3))
  }

  it must "return 3 neighbours for a boundary cell" in {
    val n = Neighbours(6, 6)
    n(0, 3) must contain(Cell(0, 2))
    n(0, 3) must contain(Cell(0, 4))
    n(0, 3) must contain(Cell(1, 3))
    n(0, 3) must not contain(Cell(-1, 3))
  }




}

object MazeGenerator {
  // Generate a square-cell maze of specified size
  def generateMaze(width:Int, height:Int): Maze = {
    val neighbours = Neighbours(width, height)

    val c = randomCell(width, height)

    val routes = process(
      initGreyListNeighbours(c, neighbours),
      initWhiteList(c, width, height),
      emptyTree(width, height),
      neighbours)

    Maze(routes)
  }

  // Use a modified Prim's algorithm to generate a maze with a route to every cell
  private def process(grey: Vector[Cell],
                      white: Array[Array[Boolean]],
                      tree: Array[Array[mutable.Set[Cell]]],
                      neighbours: Neighbours): Array[Array[mutable.Set[Cell]]] =
  {
    if (grey.isEmpty) {
      tree
    } else {
      val c = grey(r.nextInt(grey.size - 1))

      white(c.x)(c.y) = true

      // Prim's algo proper would choose single closest node. In this modified algo
      // the closest nodes are the 4 cell neighbours, all 1 unit away -- so we
      // just randomly choose one unconsidered neighbour
      val ns = neighbours(c.x, c.y).filter(n => { white(n.x)(n.y) })
      val n = if ( ns.size > 1 ) ns(r.nextInt(ns.size - 1)) else ns(0)
      tree(c.x)(c.y) += n
      tree(n.x)(n.y) += c

      process(grey.filter(t => {t != c}) union neighbours(c.x, c.y).filterNot( n => white(n.x)(n.y) ), white, tree, neighbours)
    }
  }

  private def randomCell(width:Int, height:Int): Cell = {
    Cell(r.nextInt(width - 1), r.nextInt(height - 1))
  }

  private def emptyTree(width: Int, height: Int): Array[Array[mutable.Set[Cell]]] = {
    Array.fill[mutable.Set[Cell]](width, height) { mutable.Set() }
  }

  private def initWhiteList(c: Cell, width: Int, height: Int): Array[Array[Boolean]] = {
    val white = Array.fill[Boolean](width, height) { false }

    white(c.x)(c.y) = true

    white
  }

  private def initGreyListNeighbours(c: Cell, neighbours: Neighbours): Vector[Cell] = {
    neighbours(c.x, c.y)
  }

  private val r = new Random
}

case class Cell(x:Int, y:Int) {  }

case class Neighbours(width:Int, height:Int) {
  private val neighbours = Array.fill[Vector[Cell]](width, height) {
    Vector[Cell]()
  }

  for (
    x <- 0 until width;
    y <- 0 until height
  ) {
    if (x > 0) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x - 1, y)
    if (x < width - 1) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x + 1, y)
    if (y > 0) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x, y - 1)
    if (y < height - 1) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x, y + 1)
  }

  def apply(x:Int, y:Int): Vector[Cell] = {
    neighbours(x)(y)
  }
}

case class Maze(routes: Array[Array[mutable.Set[Cell]]]) {
  val width: Int = routes.length
  val height: Int  = routes(0).length

  def apply(x: Int, y:Int): mutable.Set[Cell] = {
    routes(x)(y)
  }
}
