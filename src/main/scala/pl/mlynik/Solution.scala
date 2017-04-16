package pl.mlynik

import scala.annotation.tailrec

object Solution {
  def countPalindromicSubstrings(string: String): Int = {
    val withBoundaries = string.mkString("^#", "#", "#$")

    val n = withBoundaries.length

    val p = Array.fill(n)(0)

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
      if (i < n - 1) {
        val iprime = center - (i - center)

        val pi =
          if (rightEdge > i)
            Math.min(rightEdge - i, p(iprime))
          else
            0

        val pid = expandInitialLength(i, pi)
        val palindromicSliceCountD = palindromicSliceCount + pid / 2
        if (palindromicSliceCountD > 100000000) {
          -1
        } else {
          if (i + pid > rightEdge) {
            countPalindromicSubstrings(i + 1,
                                       center = i,
                                       rightEdge = i + pid,
                                       palindromicSliceCountD)
          } else
            countPalindromicSubstrings(i + 1,
                                       center = center,
                                       rightEdge = rightEdge,
                                       palindromicSliceCountD)
        }

      } else
        palindromicSliceCount

    countPalindromicSubstrings(1, 0, 0, 0)
  }
}
