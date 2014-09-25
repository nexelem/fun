package kata

import org.specs2.mutable.Specification

/**
 * https://www.hackerrank.com/challenges/john-and-fences
 * John's house has bizarre fencing. There are N fences. Though the contiguous fences have the constant width of 1 unit
 * but their height varies. Height of these fences is represented by array H = [h1, h2... hN].

John loves his fences but has to finally bow down to his wife's repeated requests of replacing them with the regular fences.
Before taking them down, John wants to keep some part of the fences as souvenir.
He decides to carve out the largest rectangular area possible where the largest rectangle can be made of a number of contiguous fence.
Note that sides of the rectangle should be parallel to X and Y axis.

Let's say there are 6 fences, and their height is, H = [2, 5, 7, 4, 1, 8]. Then they can be represented as

                   __
8         __      |  |
7        |  |     |  |
6      __|  |     |  |
5     |  |  |__   |  |
4     |xx|xx|xx|  |  |
3   __|xx|xx|xx|  |  |
2  |  |xx|xx|xx|__|  |
1  |__|xx|xx|xx|__|__|
    h1 h2 h3 h4 h5 h6

Some possible carvings are as follow:

    If we carve rectangle from h1, h2 and h3 then we can get the max area of 2x3 = 6 units.
    If we carve rectangle from h3, h4, h5 and h6, then max area is 4x1 = 4 units.
    If we carve rectangle from h2, h3 and h4, then max area is 4x3 = 12, which is also the most optimal solution for this case.
  (solution is marked with xx)

 */
class FencesSpec extends Specification {

  def slidingWindowWithZeros(ints: Seq[Int]): Iterator[Seq[Int]] = {
    (ints ++ Seq.fill(ints.size)(0)).sliding(ints.size)
  }

  def rectangleSizeIn(ints: Seq[Int]) : Int = {
    val input: Iterator[Seq[Int]] = slidingWindowWithZeros(ints) ++ slidingWindowWithZeros(ints.reverse)
    val mapped: Iterator[Int] = input.map(fenceHeights => (fenceHeights.tail.takeWhile(height => height >= fenceHeights.head).size +1) * fenceHeights.head)
    mapped.max
  }

  "john" should {
    "find the biggest rectangle in a fence" in {
      rectangleSizeIn(Seq(2, 5, 7, 4, 1, 8)) must beEqualTo(12)
    }
  }

}
