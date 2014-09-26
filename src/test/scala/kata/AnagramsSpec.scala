package kata

import org.specs2.mutable.Specification

class AnagramsSpec extends Specification {

  /**
   * Implement me here
   */
  def anagramsFor(input: Seq[String]): Set[Set[String]] = {
    val setOfAnagrams = input.toSet.groupBy((word: String) => word.sorted).values.toSet
    setOfAnagrams.filter(_.size > 1)
  }

  "anagram finder" should {

    "find all anagrams in extremely short list" in {
      val anagrams = anagramsFor(Seq("rots", "sort", "NOT"))

      anagrams must containAllOf(
        Seq(
          Set("rots", "sort")
        )
      )
    }

    "find all anagrams in short list" in {
      val anagrams = anagramsFor(Seq("kinship", "pinkish", "enlist", "inlets", "listen", "silent", "boaster", "boaters", "borates", "fresher", "refresh", "sinks", "skins", "knits", "stink", "rots", "sort", "NOT"))

      anagrams must containAllOf(
        Seq(
          Set("kinship", "pinkish"),
          Set("enlist", "inlets", "listen", "silent"),
          Set("boaster", "boaters", "borates"),
          Set("fresher", "refresh"),
          Set("sinks", "skins"),
          Set("knits", "stink"),
          Set("rots", "sort")
        )
      )
    }

    "find all anagrams in long list" in {
      def lines(fileName: String) : Seq[String] = {
        io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(fileName)).getLines().toSeq
      }
      val allAnagrams: Set[Set[String]] = anagramsFor(lines("wordlist.txt"))

      allAnagrams.take(10).foreach(println)

      allAnagrams.size must beEqualTo(20683)
      allAnagrams.contains(Set("crepitus", "cuprites", "pictures", "piecrust")) must beTrue
      allAnagrams.contains(Set("paste", "pates", "peats", "septa", "spate", "tapes", "tepas")) must beTrue
      allAnagrams.contains(Set("punctilio", "unpolitic")) must beTrue
      allAnagrams.contains(Set("sunders", "undress")) must beTrue
    }
  }


}
