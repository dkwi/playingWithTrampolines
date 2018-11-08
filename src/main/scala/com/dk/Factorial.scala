package com.dk

object Factorial extends App {

  def fac(n: Int): Trampoline[Int] =
    if (n == 0) Return(1)
    else Suspend(() ⇒ fac(n - 1)).flatMap(x ⇒ Return(n * x))


  def unsafeFac(n: Int): Int =
    if (n == 0) 1
    else n * unsafeFac(n - 1)


  val i =9000000
  println(Trampoline.run(fac(i)))
  println(unsafeFac(i))
}


