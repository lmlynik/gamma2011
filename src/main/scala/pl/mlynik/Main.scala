package pl.mlynik

object Main extends App {
  args.toList match {
    case a :: xs =>
      val result = Solution.countPalindromicSubstrings(a)
      println(s"$a has $result palindromes")
    case Nil =>
      sys.error("Missing input parameter")
      sys.exit(1)
  }
}
