package com.mlynik

import scala.collection.mutable.ArrayBuffer

/**
  * Created by lukaszmlynik on 4/13/17.
  */
object Main extends App {
  val string = args lift 0 getOrElse "baababa"
  val withBoundaries = string.mkString("^#", "#", "#$")

  val n = withBoundaries.length

  @scala.annotation.tailrec
  def expandBeyondInitialLength(str: String, i: Int, pid: Int): Int =
    if (str.charAt(i + 1 + pid) == str.charAt(i - 1 - pid))
      expandBeyondInitialLength(str, i, pid + 1)
    else
      pid

  def f(i: Int = 1,
        c: Int = 0,
        r: Int = 0,
        palindromicSliceCount: Int = 0,
        p: ArrayBuffer[Int] = ArrayBuffer.fill(n)(0)): Int =
    if (i < n - 1) {

      val iprime = c - (i - c)

      val pi =
        if (r > i)
          Math.min(r - i, p(iprime))
        else
          0

      val pid = expandBeyondInitialLength(withBoundaries, i, pi)
      val palindromicSliceCointD = palindromicSliceCount + pid / 2

      if (palindromicSliceCointD > 100000000) {
        -1
      } else {
        if (i + pid > r) {
          f(i + 1, c = i, r = i + pid, palindromicSliceCointD, p :+ pid)
        } else
          f(i + 1, c = c, r = r, palindromicSliceCointD, p :+ pid)
      }

    } else
      palindromicSliceCount

  println(f())

}
