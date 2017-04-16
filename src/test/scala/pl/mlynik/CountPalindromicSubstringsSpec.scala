package pl.mlynik

import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CountPalindromicSubstringsSpec
    extends WordSpec
    with GeneratorDrivenPropertyChecks {

  val abcStringGen = for {
    listSize <- Gen.choose(4, 20000)
    list <- Gen.listOfN(listSize, Gen.oneOf('a', 'b', 'c'))
  } yield list.mkString

  "Solution" should {

    forAll(abcStringGen) { (testString) =>
      s"count palindromes in a string consisting of only ABC of size ${testString.length}" in {
        val solution = Solution.countPalindromicSubstrings(testString)
        assert(solution >= 0)
      }
    }

    forAll(Gen.alphaLowerStr) { (testString) =>
      s"count palindromes in $testString" in {
        val solution = Solution.countPalindromicSubstrings(testString)
        assert(solution >= 0)
      }
    }

    "handle very long palindrome should return -1" in {
      val veryLongLotsOfPalindromes = Seq.fill(20000)("a").mkString
      val solution =
        Solution.countPalindromicSubstrings(veryLongLotsOfPalindromes)
      assert(solution == -1)
    }

    "handle 0 length strings" in {
      val solution =
        Solution.countPalindromicSubstrings("")
      assert(solution == 0)
    }

    "handle 1 length strings" in {
      val solution =
        Solution.countPalindromicSubstrings("a")
      assert(solution == 0)
    }
  }
}
