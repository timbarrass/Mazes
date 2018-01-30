package com.timbarrass.mazes

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random
import org.scalatest

/*
Maze
Generate a random maze - do I need the final maze expressed as routes?
Transform that maze to a final block grid, handling corridor width
How big will transformed maze be?
Break a link (relationship) (break a random link -- need to get handed a maze object we can modify or modify own state)
Break a link (final block grid)
Make a link (relationship)
Make a link (final block grid)
Flood fill
Random adjust - break, identify and make links
Let me know if the maze changes

Options
Toolkit - provide operations and define objects to be passed around but don't keep a record of what's created
Alt - all ops result in a final block grid rather than

Route Generator
Flood Filler
Cell
Routes (finalMaze atm - do we need to expose this?
Transformed (finalGrid atm - typically what client is using)
 */


object Driver extends App {

  val width = 10
  val height = 10
  val scale = 1

  val m = new PrimsMaze(width, height, scale  )

  display(m)

  m.breakALink()

  display(m)

  val finalFlooded = m.floodFill

  displayFlooded(m, finalFlooded._1)

  println("size of black: " + finalFlooded._2.size)

  def displayFlooded(m: PrimsMaze, flood:Array[Array[Int]]): Unit = {
    val f = m.finalGrid

    for (
      y <- 0 until (height * (scale + 1) + 1);
      x <- 0 until (width * (scale + 1) + 1);
      _ = if (x == 5) { println }
    ) {
      if (f(x)(y)) {
        print(1.toChar)
      } else {
        print(flood(m.reverseTransform(x, scale))(m.reverseTransform(y, scale)))
      }
    }
    println
  }



  def display(m: PrimsMaze): Unit = {
   val f = m.finalGrid

    for (
      y <- 0 until (height * (scale + 1) + 1);
      x <- 0 until (width * (scale + 1) + 1);
      _ = if (x == 5) { println }
    ) {
      if (f(x)(y)) {
        print(1.toChar)
      } else {
        print(" ")
      }
    }
    println
  }
}




class PrimsMaze(width:Int, height:Int, scale: Int) {
  private val r = new Random
  private val neighbours = initNeighbours // neighbours is basically the static config that makes this a maze solver

  val finalMaze = process(randomCell(width, height))

  def finalGrid: Array[Array[Boolean]] = {
    transformToFinalGrid(finalMaze)
  }

  def fullFinalGrid: Array[Array[Boolean]] = {
    Array.fill[Boolean](width * (scale + 1) + 1, height * (scale + 1) + 1) { true } // true => wall
  }

  private def emptyTree: Array[Array[mutable.Set[Cell]]] = {
    Array.fill[mutable.Set[Cell]](width, height) { Set() }
  }

  def breakALink(): Unit = {
    val r = Random
    val x = r.nextInt(width - 1)
    val y = r.nextInt(height - 1)
    println((x,y))
    finalMaze(x)(y).foreach(c => println((c.x,c.y)))
    val targetCell = finalMaze(x)(y).head // need to sort out this random set member choosing
    println(x + " " + y + " " + targetCell.x + " " + targetCell.y)
    finalMaze(x)(y).remove(targetCell)
    finalMaze(targetCell.x)(targetCell.y).remove(new Cell(x, y))
  }

  def floodFill: (Array[Array[Int]], Set[Cell]) = {
    val setId = 1
    val empty = Array.fill[Int](width, height) {
      0
    }
    val black = Set[Cell]()
    val grey = Set[Cell]()

    for {
      y <- 0 until height;
      x <- 0 until width
    } {
      black += new Cell(x, y)
    }

    val r = Random

    val c = black.head
    black -= c
    grey += c
    val finalFlooded = processFloodFill(grey, black, finalMaze, setId, empty)

    finalFlooded
  }

  def processFloodFill(grey: Set[Cell], black: Set[Cell], tree:Array[Array[Set[Cell]]], setId: Int, flooded: Array[Array[Int]]): (Array[Array[Int]], Set[Cell]) = {
    if(grey.size == 0) {
      (flooded, black)
    } else {
      val c = grey.head // random?
      grey -= c

      flooded(c.x)(c.y) = setId

      processFloodFill(grey union tree(c.x)(c.y) intersect black, black diff tree(c.x)(c.y), tree, setId, flooded)
    }
  }

  // final grid coordinates to final maze
  def reverseTransform(x:Int, scale:Int): Int = {
    (x - 1) / (scale + 1)
  }

  def transformToFinalGrid(finalMaze: Array[Array[mutable.Set[Cell]]]): Array[Array[Boolean]] = {
    transformToFinalGrid(finalMaze, fullFinalGrid)
  }

  def transformToFinalGrid(finalMaze: Array[Array[mutable.Set[Cell]]], finalGrid: Array[Array[Boolean]]): Array[Array[Boolean]] = {

    for (
      y <- 0 until height;
      x <- 0 until width
    ) {

      // clear out the cell first
      for (
        yStep <- 1 until scale + 1;
        xStep <- 1 until scale + 1
      ) {
        finalGrid(transform(x, scale, xStep))(transform(y, scale, yStep)) = false // false => no wall
      }

      for (n <- finalMaze(x)(y)) {
        // clear x-axis routes
        if (n.x == x) {
          val yWall = (scale + 1) * math.max(n.y, y)
          val xWallStart = (scale + 1) * n.x + 1

          for (xWall <- xWallStart until xWallStart + scale) {
            finalGrid(xWall)(yWall) = false
          }
        }

        // clear y-axis routes
        if (n.y == y) {
          val yWallStart = (scale + 1) * n.y + 1
          val xWall = (scale + 1) * math.max(n.x, x)

          for (yWall <- yWallStart until yWallStart + scale) {
            finalGrid(xWall)(yWall) = false
          }
        }
      }
    }

    // clear an entrance
    val entrance: Int = (scale + 1) * Random.nextInt(width) + 1
    for (x <- entrance until entrance + scale) {
      finalGrid(x)(0) = false
    }

    finalGrid
  }

  private def transform(x: Int, scale: Int, step: Int): Int = {
    (scale + 1) * x + step
  }

  def process(c: Cell) : Array[Array[mutable.Set[Cell]]] = {
    process(initGreyListNeighbours(c), initWhiteList(c))
  }

  def process(grey: mutable.Set[Cell], white: Array[Array[Boolean]]): Array[Array[mutable.Set[Cell]]] = {
    process(grey, white, emptyTree)
  }

  def process(grey: mutable.Set[Cell], white: Array[Array[Boolean]], tree: Array[Array[mutable.Set[Cell]]]): Array[Array[mutable.Set[Cell]]] =
  {
    if (grey.isEmpty) {
      tree
    } else {
      Random.shuffle(grey) // ha so efficient
      val c = grey.head // random?
      grey -= c

      white(c.x)(c.y) = true

      // Prim's algo proper would choose single closest node. In this modified algo
      // the closest nodes are the 4 cell neighbours, all 1 unit away -- so we
      // just randomly choose one unconsidered neighbour
      val n = neighbours(c.x)(c.y).filter(n => { white(n.x)(n.y) }).head
      tree(c.x)(c.y) += n
      tree(n.x)(n.y) += c

      process(grey union neighbours(c.x)(c.y).filterNot( n => white(n.x)(n.y) ), white, tree)
    }
  }

  private def initNeighbours: Array[Array[mutable.Set[Cell]]] = {
    val neighbours = Array.fill[mutable.Set[Cell]](width, height) {
      Set()
    }

    for (
      x <- 0 until width;
      y <- 0 until height
    ) {
      if (x > 0) neighbours(x)(y) += Cell(x - 1, y)
      if (x < width - 1) neighbours(x)(y) += Cell(x + 1, y)
      if (y > 0) neighbours(x)(y) += Cell(x, y - 1)
      if (y < height - 1) neighbours(x)(y) += Cell(x, y + 1)
    }

    neighbours
  }

  private def randomCell(width:Int, height:Int): Cell = {
    Cell(r.nextInt(width - 1), r.nextInt(height - 1))
  }

  private def initWhiteList(c: Cell): Array[Array[Boolean]] = {
    val white = Array.fill[Boolean](width, height) { false }

    white(c.x)(c.y) = true

    white
  }

  private def initGreyListNeighbours(c: Cell): mutable.Set[Cell] = {
    val grey = Set[Cell]()

    for (n <- neighbours(c.x)(c.y)) {
      if (!grey.contains(n)) {
        grey += n
      }
    }

    grey
  }
}
