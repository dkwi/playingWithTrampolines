package com.dk

sealed trait Trampoline[A] {
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = FlatMap(this, f)
}

object Trampoline {
  def run[A](tr: Trampoline[A]): A = tr match {
    case Return(a) => a
    case Suspend(s) => run(s())
    case FlatMap(t, f) => t match {
      case Return(a) => run(f(a))
      case Suspend(s) => run(s().flatMap(f))
      case FlatMap(innerT, g) => run(innerT.flatMap(x ⇒ g(x) flatMap f))
    }
  }
}

final case class Return[A](a: A) extends Trampoline[A]

final case class Suspend[A](s: () => Trampoline[A]) extends Trampoline[A]

final case class FlatMap[A, B](sub: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]