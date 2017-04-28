package pl.mlynik

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Solution {

  def countPalindromicSubstrings(string: String): Int = {
    val withBoundaries = string.mkString("^#", "#", "#$")

    val n = withBoundaries.length

    val p = ArrayBuffer.fill(n)(0)

    @tailrec
    def expandInitialLength(i: Int, pid: Int): Int =
      if (withBoundaries.charAt(i + 1 + pid) == withBoundaries.charAt(
            i - 1 - pid))
        expandInitialLength(i, pid + 1)
      else
        pid

    /**
      * Recursive function for calculating count of palindromic substrings
      * @param i iterator
      * @param center center of palindrome
      * @param rightEdge right edge of the palindrome
      * @param palindromicSliceCount accumulator counter palindromes
      * @return
      */
    @tailrec
    def countPalindromicSubstrings(i: Int,
                                   center: Int,
                                   rightEdge: Int,
                                   palindromicSliceCount: Int): Int =
      if (palindromicSliceCount > 100000000) {
        -1
      } else if (i < n - 1) {
        val iprime = center - (i - center)

        val pi =
          if (rightEdge > i)
            Math.min(rightEdge - i, p(iprime))
          else
            0

        p(i) = expandInitialLength(i, pi)
        val palindromicSliceCountD = palindromicSliceCount + p(i) / 2

        if (i + p(i) > rightEdge) {
          countPalindromicSubstrings(i + 1,
                                     center = i,
                                     rightEdge = i + p(i),
                                     palindromicSliceCountD)
        } else
          countPalindromicSubstrings(i + 1,
                                     center = center,
                                     rightEdge = rightEdge,
                                     palindromicSliceCountD)

      } else {
        palindromicSliceCount
      }

    countPalindromicSubstrings(1, 0, 0, 0)
  }
}
