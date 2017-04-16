package pl.mlynik

object Main extends App {
  args.toList match {
    case a :: xs =>
      println(Solution.countPalindromicSubstrings(a))
    case Nil =>
      sys.error("Missing input parameter")
      sys.exit(1)
  }
}
