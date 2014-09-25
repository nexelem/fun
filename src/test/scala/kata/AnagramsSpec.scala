package kata

import org.specs2.mutable.Specification

class AnagramsSpec extends Specification {

  /**
   * Implement me here
   */
  def anagramsFor(input: Seq[String]): Set[Set[String]] = ???

  "anagram finder" should {
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

      allAnagrams.size must beEqualTo(20683)
      allAnagrams must containAllOf(
        Seq(
            Set("crepitus", "cuprites", "pictures", "piecrust"),
            Set("paste", "pates", "peats", "septa", "spate", "tapes", "tepas"),
            Set("punctilio", "unpolitic"),
            Set("sunders", "undress")
        )
      )
    }
  }


}
