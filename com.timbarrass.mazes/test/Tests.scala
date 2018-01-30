import com.timbarrass.mazes.{Cell, Maze, Neighbours}
import org.scalatest.{FlatSpec, MustMatchers}

import scala.collection.mutable

class MazeTest extends FlatSpec with MustMatchers {

  behavior of "Maze"

  "generateMaze" must "return a new Maze when requested" in {
    Maze.generateMaze(3, 4) mustBe an[Maze]
  }

  it must "return a Maze with specified width and height" in {
    val m = Maze.generateMaze(3, 4)
    m.width must equal(3)
    m.height must equal(4)
  }

  "maze" should "return a set of neighbour cells for a given cell" in {
    val m = Maze.generateMaze(4, 4)
    m(2, 3) mustBe an[mutable.Set[_]]
  }

  it must "have at least one route out of each cell" in {
    val m = Maze.generateMaze(4, 4)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      m(x, y).size must be > 0
    }
  }

  it must "have a matching route in for each route out of a cell" in {
    val m = Maze.generateMaze(4, 4)
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

