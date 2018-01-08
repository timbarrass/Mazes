package com.timbarrass.mazes

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

case class Cell(xIn:Int, yIn: Int) {
  val x = xIn
  val y = yIn
}

object Driver extends App {
  val width = 20
  val height = 20

  val m = new PrimsMaze(width, height)

  for ( y <- 0 to ( height * 2 )) {
    for ( x <- 0 to ( width * 2 )) {
      print(m.finalGrid(x)(y))
    }
    println()
  }

}

class PrimsMaze(w:Int, h:Int) {
  val r = new Random

  val width  = w
  val height = h

  val black = Array.fill[String](width, height) { "#" }

  val white = Array.fill[Boolean](width, height) { false }

  val grey = Set[Cell]()

  val neighbours = Array.fill[mutable.Set[Cell]](width, height) { Set() }
  val tree = Array.fill[mutable.Set[Cell]](width, height) { Set() }

  for ( x <- 0 to width - 1 ) {
    for ( y <- 0 to height - 1 ) {
      if (x > 0)          neighbours(x)(y) += Cell(x-1, y)
      if (x < width - 1)  neighbours(x)(y) += Cell(x+1, y)
      if (y > 0)          neighbours(x)(y) += Cell(x, y-1)
      if (y < height - 1) neighbours(x)(y) += Cell(x, y+1)
    }
  }

  chooseFirstCell

  while ( grey.size > 0 ) {
    // choose a random grey cell
    Random.shuffle(grey) // ha so efficient
    val c = grey.head // random?
    grey -= c

    // move cell into white set
    white(c.x)(c.y) = true
    black(c.x)(c.y) = " "

    // choose a neighbour of cell that's in white to go into tree
    // this is where Prim proper would use a shortest metric to choose
    // depending and random iteration through set here ...
    tree(c.x)(c.y) += neighbours(c.x)(c.y).filter(n => { if (white(n.x)(n.y)) { true } else { false }}).head

    // shift neighbours of new white cell into grey set
    for ( n <- neighbours(c.x)(c.y)) {
      if ( ! white(n.x)(n.y) && ! grey.contains(n) ) {
        grey += n
        black(n.x)(n.y) = "F"
      }
    }
  }

  val finalGrid = Array.fill[String](width * 2 + 1, height * 2 + 1) { "#" }

  for ( y <- 0 to height - 1 ) {
    for ( x <- 0 to width - 1 ) {
      finalGrid(x*2 + 1)(y*2 + 1) = " "
      for(n <- tree(x)(y)) {
        finalGrid((x * 2) + (1 + n.x - x))((y * 2) + (1 + n.y - y)) = " "
      }
    }
  }

  // poke two holes
  val top:Int = r.nextInt(width)
  val bottom:Int = r.nextInt(width)
  finalGrid(top * 2 + 1)(0) = " "
  finalGrid(bottom * 2 + 1)(height * 2) = " "


  private def display: Unit = {
    for (y <- 0 to height - 1) {
      for (x <- 0 to width - 1) {
        print(black(x)(y))
      }
      println()
    }

    println("---------------")

    for (y <- 0 to height - 1) {
      for (x <- 0 to width - 1) {
        if (white(x)(y)) print("#") else print(" ")
      }
      println()
    }

    for (g <- grey) {
      println("grey: " + g.x + " " + g.y)
    }
  }

  private def chooseFirstCell: Unit = {

    var c = Cell(r.nextInt(width - 1), r.nextInt(height - 1))

    black(c.x)(c.y) = " "

    white(c.x)(c.y) = true

    for (n <- neighbours(c.x)(c.y)) {
      if (!grey.contains(n)) {
        grey += n
        black(n.x)(n.y) = "F"
      }
    }
  }

}
