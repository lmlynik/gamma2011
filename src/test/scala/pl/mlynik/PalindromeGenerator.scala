package pl.mlynik

object PalindromeGenerator {
  /**
    * Generate a string containing [n] palindromes by creating a string and mirroring it
    * @param n count of palindromes in the string
    * @return a string containing [n] palindromes
    */
  def generateMirrored(n: Int): String = {
    val left = (1 until n).map(_.toChar).mkString
    val mirroredString = left + n.toChar + n.toChar + left.reverse
    mirroredString
  }
}
