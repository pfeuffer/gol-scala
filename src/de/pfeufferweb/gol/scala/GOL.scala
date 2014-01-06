package de.pfeufferweb.gol.scala

import de.pfeufferweb.gol.benchmark.Gol
import de.pfeufferweb.gol.benchmark.GolBuilder

class GOL(cells: Set[Cell]) extends Gol {
  def next: GOL = {
    val cellsStayingAlive = cells.filter(staysAlive(_))
    val cellsReborn = cells.flatMap(neighbours).filter(aliveNeighbourCount(_) == 3)
    new GOL(cellsStayingAlive ++ cellsReborn)
  }
  def isAlive(x: Int, y: Int) = {
    cells.contains(Cell(x, y))
  }
  private def staysAlive(cell: Cell) = {
    val count = aliveNeighbourCount(cell)
    count == 2 || count == 3
  }
  private def aliveNeighbourCount(cell: Cell) = {
    aliveNeighbours(cell).size
  }
  private def aliveNeighbours(cell: Cell) = {
    neighbours(cell).filter(cells.contains(_))
  }
  private def neighbours(cell: Cell) = {
    for {
      x <- cell.x - 1 to cell.x + 1
      y <- cell.y - 1 to cell.y + 1
      if (x != cell.x || y != cell.y)
    } yield Cell(x, y)
  }
}

class GOLBuilder extends GolBuilder {
  var cells: Set[Cell] = Set()
  def addCell(x: Int, y: Int) = {
    cells = cells + new Cell(x, y)
  }

  def create: Gol = {
    new GOL(cells)
  }
}

case class Cell(x: Int, y: Int)