package kata

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations

/**
 *
 * http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
 *
 * The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, alive or dead. Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:

    Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any live cell with more than three live neighbours dies, as if by overcrowding.
    Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

 */
class GameOfLifeSpec extends Specification {

  case class Cord(x: Int, y: Int) {
  }

  case class World(alive: Set[Cord]) {

    def next : World = ???

    def cellAlive(cord: Cord) = alive.contains(cord)
  }

  object World {

    def apply(aliveCellCords: Cord*) = new World(aliveCellCords.toSet)
  }

  "game" should {
    "say wether cell is alive" in {
      val world = World()

      world.cellAlive(Cord(0, 0)) must beFalse
    }

    "keep alive cell" in {
      val world = World(Cord(0, 0))

      world.cellAlive(Cord(0, 0)) must beTrue
    }

    "kill alive cell if no neighbours" in {
      val world = World(Cord(0, 0))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "keep cell alive when two neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "keep cell alive when three neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0), Cord(1, 1))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "die when four neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0), Cord(0, 1), Cord(0, -1))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "three alive cells reproduce new cell" in {
      val world = World(Cord(-1, 0), Cord(1, 0), Cord(0, 1))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "two alive cells will not reproduce new cell" in {
      val world = World(Cord(-1, 0), Cord(0, 1))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "three alive cells ina row reproduce new cells" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0))

      world.next.cellAlive(Cord(0, 1)) must beTrue
      world.next.cellAlive(Cord(0, -1)) must beTrue
    }
  }


}
