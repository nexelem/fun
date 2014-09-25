package kata

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations

class FibonacciSpec extends Specification {

  lazy val fibonacci: Stream[Long] = ???

  "fibbonacci" should {
    Seq(
      1 -> 1,
      2 -> 1,
      3 -> 2,
      4 -> 3,
      5 -> 5,
      6 -> 8,
      7 -> 13,
      46 -> 1836311903,
      92 -> 7540113804746346429L
    ) foreach { case (input, expected) =>
      s"return $expected for $input" in {

        fibonacci(input - 1) must beEqualTo(expected)
      }
    }
  }


}
