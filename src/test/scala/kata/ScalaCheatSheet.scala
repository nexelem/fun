package kata

import org.specs2.mutable.Specification

import scala.annotation.tailrec

class ScalaCheatSheet extends Specification {

  "Scala programmer" should {
    "define values and variables" in {
      val int = 1
      val str = "hello world"
      var variable = Seq(1)
      variable = Seq(2)

      s"string $str integer $int var $variable" must beEqualTo("string hello world integer 1 var List(2)")
    }

    "define and call methods" in {
      def method(param1: Int, param2: String): Seq[Int] = Seq(param1, param2.toInt)
      def paramlessMethod: String = {
        val returnValue = "2"
        returnValue
      }

      method(1, paramlessMethod) must beEqualTo(Seq(1, 2))
    }

    "use if as an expression" in {
      val input = 1

      val ifValue: String = if (input == 1) "equal to 1" else "not equal"

      ifValue must beEqualTo("equal to 1")
    }

    "pass functions, methods and anonymous functions" in {
      def isPlayerGood(player: String, scoreFunction: String => Int) = scoreFunction(player) > 10
      val asGoodAsLenghtOfName = (player: String) => player.length
      def fixedScore(player: String) = 5

      isPlayerGood("A", asGoodAsLenghtOfName) must beFalse
      isPlayerGood("playerWithVeryLongName", asGoodAsLenghtOfName) must beTrue
      isPlayerGood("a", fixedScore) must beFalse
      isPlayerGood("a", (player) => 20) must beTrue
    }

    "create list" in {
      val seq: Seq[Int] = Seq(1, 2, 3, 4)

      seq.size must beEqualTo(4)
      seq.head must beEqualTo(1)
      seq.tail must beEqualTo(Seq(2, 3, 4))
    }

    "create map" in {
      val map: Map[Int, String] = Map(1 -> "wow", 2 -> "hmm")

      map.size must beEqualTo(2)
      map(1) must beEqualTo("wow")
      map.getOrElse(3, "unknown") must beEqualTo("unknown")
    }

    "create option" in {
      def positive(value: Int) = if (value > 0) Some(value) else None
      val someOption = positive(5)
      val noneOption = positive(-5)

      someOption.get must beEqualTo(5)
      noneOption.getOrElse(0) must beEqualTo(0)
      someOption.contains(5) must beTrue
      noneOption.contains(5) must beFalse
      someOption.isDefined must beTrue
      noneOption.isDefined must beFalse
    }

    "create tuples" in {
      val tuple: (Int, String) = (1,"v")

      tuple._1 must beEqualTo(1)
      tuple._2 must beEqualTo("v")
    }

    "use high order functions" in {
      val seq = (1 to 5).toSeq

      seq.map(int => int * 2) must beEqualTo(Seq(2, 4, 6, 8, 10))
      seq.filter(int => int < 3) must beEqualTo(Seq(1, 2))
      seq.count(int => int > 3) must beEqualTo(2)
      seq.dropWhile(int => int < 5) must beEqualTo(Seq(5))
      seq.exists(int => int == 6) must beFalse

      seq.foreach(int => println(int) )
      seq.groupBy(int => int % 2) must beEqualTo(Map(0 -> Seq(2,4),1->Seq(1,3,5)))
      seq.sorted(Ordering[Int].reverse) must beEqualTo(Seq(5,4,3,2,1))
      val reversed = seq.reverse
      seq.zip(reversed) must beEqualTo(Seq((1,5),(2,4),(3,3),(4,2),(5,1)))
    }

    "use more high order functions" in {
      val seq = (1 to 5).toSeq

      seq.reduce((int1, int2) => int1 + int2) must beEqualTo(15)
      seq.sum must beEqualTo(15)

      seq.foldLeft(Integer.MIN_VALUE)((int1, int2) => {
        if (int2 > int1) int2 else int1
      }) must beEqualTo(5)
      seq.max must beEqualTo(5)

      seq.flatMap(int => Seq(1,int)) must beEqualTo(Seq(1,1,1,2,1,3,1,4,1,5))
      seq.map(int => Seq(1,int)).flatten must beEqualTo(Seq(1,1,1,2,1,3,1,4,1,5))
    }

    "use more high order functions" in {
      val seq = (1 to 5).toSeq

      seq.reduce((int1, int2) => int1 + int2) must beEqualTo(15)
      seq.sum must beEqualTo(15)

      seq.foldLeft(Integer.MIN_VALUE)((int1, int2) => {
        if (int2 > int1) int2 else int1
      }) must beEqualTo(5)
      seq.max must beEqualTo(5)

      seq.flatMap(int => Seq(1,int)) must beEqualTo(Seq(1,1,1,2,1,3,1,4,1,5))
      seq.map(int => Seq(1,int)).flatten must beEqualTo(Seq(1,1,1,2,1,3,1,4,1,5))
    }

    "use lazy collection with iterate" in {
      val doubledNumbersStream: Stream[BigInt] = Stream.iterate(start = BigInt(1))(previous => previous * BigInt(2))

      doubledNumbersStream.take(1) must beEqualTo(Seq(1))
      doubledNumbersStream.take(5) must beEqualTo(Seq(1,2,4,8,16))
      doubledNumbersStream(200) must beEqualTo(BigInt("1606938044258990275541962092341162602522202993782792835301376"))
    }

    "use lazy collection" in {
      def doubledNumberStreamFrom(int: BigInt) : Stream[BigInt] = int #:: doubledNumberStreamFrom(int * BigInt(2))
      val doubledNumbersStream: Stream[BigInt] = doubledNumberStreamFrom(BigInt(1))

      doubledNumbersStream.take(1) must beEqualTo(Seq(1))
      doubledNumbersStream.take(5) must beEqualTo(Seq(1,2,4,8,16))
      doubledNumbersStream(200) must beEqualTo(BigInt("1606938044258990275541962092341162602522202993782792835301376"))
    }

    "use recursion with optional tailrec" in {
      @tailrec
      def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

      def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

      gcd(10, 22) must beEqualTo(2)
      factorial(7) must beEqualTo(5040)
    }

    "use case classes" in {
      case class Person(name: String)

      Person("Ken") must beEqualTo(Person("Ken"))
      Person("Ken").copy(name="Barbie") must beEqualTo(Person("Barbie"))
      Person("Ken").name must beEqualTo("Ken")
    }


  }

}
