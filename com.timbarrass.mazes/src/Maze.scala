package com.timbarrass.mazes

import scala.collection.mutable
import scala.util.Random

object Maze {
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

case class Maze(routes: Array[Array[mutable.Set[Cell]]]) {
  val width: Int = routes.length
  val height: Int  = routes(0).length

  def apply(x: Int, y:Int): mutable.Set[Cell] = {
    routes(x)(y)
  }
}


