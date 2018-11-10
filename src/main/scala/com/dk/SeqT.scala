package com.dk

import cats._
import cats.implicits._

import scala.language.higherKinds


trait SeqT[S[_], T] {

  def add(s: S[T], t: T): S[T]

  def concat(a: S[T], b: S[T]): S[T]

  def size(s: S[_]): Int

  def empty: S[T]

  def unapply(s: S[T]): Option[(T, S[T])]

  def fromRange(range: Range): S[Int]

  def map[A, B](s: S[A])(f: A ⇒ B): S[B]

  def slice(s: S[T])(from: Int, until: Int): S[T]

  def apply(t: T*): S[T]
}

object SeqT {

  implicit def listSeqT[T]: SeqT[List, T] = new SeqT[List, T] {
    override def add(s: List[T], t: T): List[T] = t :: s

    override def concat(a: List[T], b: List[T]): List[T] = a ++ b

    override def size(s: List[_]): Int = s.length

    override def empty: List[T] = Nil

    override def unapply(s: List[T]): Option[(T, List[T])] = if (s.nonEmpty) Some(s.head -> s.tail) else None

    override def fromRange(range: Range): List[Int] = range.toList

    override def map[A, B](s: List[A])(f: A => B): List[B] = s map f

    override def slice(s: List[T])(from: Int, until: Int): List[T] = s slice(from, until)

    override def apply(t: T*): List[T] = t.toList
  }

  implicit def streamSeqT[T]: SeqT[Stream, T] = new SeqT[Stream, T] {
    override def add(s: Stream[T], t: T): Stream[T] = t +: s

    override def concat(a: Stream[T], b: Stream[T]): Stream[T] = a ++ b

    override def size(s: Stream[_]): Int = s.length

    override def empty: Stream[T] = Stream()

    override def unapply(s: Stream[T]): Option[(T, Stream[T])] = if (s.nonEmpty) Some(s.head -> s.tail) else None

    override def fromRange(range: Range): Stream[Int] = range.toStream

    override def map[A, B](s: Stream[A])(f: A => B): Stream[B] = s map f

    override def slice(s: Stream[T])(from: Int, until: Int): Stream[T] = s slice(from, until)

    override def apply(t: T*): Stream[T] = t.toStream
  }


}

object SeqTMain extends App {
  def myGen[C[_], A](seq: C[A], size: Int)(implicit C: SeqT[C, A], CC: SeqT[C, C[A]]): C[C[A]] = {
    val seqSize = C.size(seq)

    def loop(seq: C[A], size: Int): Trampoline[C[C[A]]] = {
      def allInserts(x: A, ll: C[C[A]]): Trampoline[C[C[A]]] = {
        ll match {
          case CC(hd, tl) ⇒
            Suspend(() ⇒ allInserts(x, tl))
              .flatMap(all ⇒ Return(CC.concat(inserts(x, hd), all)))

          case _ ⇒ Return(CC.empty)
        }
      }

      def inserts(x: A, ll: C[A]): C[C[A]] =
        C.map(C.fromRange(0 to C.size(ll))) { i ⇒ insertOne(i, x, ll) }

      def insertOne(i: Int, x: A, ll: C[A]): C[A] =
        C.concat(C.slice(ll)(0, i), C.add(C.slice(ll)(i, C.size(ll)), x))

      seq match {
        case _ if size > seqSize ⇒ Return(CC.empty)
        case _ if size == 1 ⇒ Return(C.map(seq)(C(_)))
        case C(hd, tl) ⇒
          for {
            subList <- Suspend(() ⇒ loop(tl, size - 1))
            acc <- Suspend(() ⇒ loop(tl, size))
            all <- allInserts(hd, subList)
          } yield CC.concat(all, acc)
        case _ ⇒ Return(CC.empty)
      }
    }

    Trampoline.run(loop(seq, size))
  }


  def catsPerm[C[_], A](seq: C[A], size: Int)(implicit FF: FunctorFilter[C], Tr: Traverse[C], Mk: MonoidK[C], M: Monad[C]): C[C[A]] = {
    val seqSize = Tr.size(seq)

    def unapply[Q[_], B](t: Q[B])(implicit FFq: FunctorFilter[Q], Tq: Traverse[Q], Mq: Monad[Q]) = {
      val zipped = Tq.zipWithIndex(t)
      Tq.find(zipped)(_._2 == 0).get._1 -> Mq.map(FFq.filter(zipped)(_._2 != 0))(_._1)
    }

    def loop(seq: C[A], size: Int): Trampoline[C[C[A]]] = {
      def allInserts(x: A, ll: C[C[A]]): Trampoline[C[C[A]]] = {
        ll match {
          case t if Tr.nonEmpty(t) ⇒
            val (hd, tl) = unapply(t)
            Suspend(() ⇒ allInserts(x, tl))
              .flatMap(all ⇒ Return(Mk.combineK(inserts(x, hd), all)))

          case _ ⇒ Return(Mk.empty)
        }
      }

      def inserts(x: A, ll: C[A]): C[C[A]] = {
        val llSize = Tr.size(ll)

        def itr(acc: C[Int], j: Int = 0): C[Int] = {
          if (j > llSize) acc
          else itr(Mk.combineK(acc, M.point(j)), j + 1)
        }

        M.map(itr(Mk.empty))(i ⇒ insertOne(i, x, ll))
      }

      def insertOne(i: Int, x: A, ll: C[A]): C[A] = {
        val zipped = Tr.zipWithIndex(ll)
        Mk.combineK(Mk.combineK(M.map(FF.filter(zipped)(_._2 < i))(_._1), M.point(x)), M.map(FF.filter(zipped)(_._2 >= i))(_._1))
      }

      seq match {
        case _ if size > seqSize ⇒ Return(Mk.empty)
        case _ if size == 1 ⇒ Return(M.map(seq)(M.point))
        case t if Tr.nonEmpty(t) ⇒
          val (hd, tl) = unapply(t)
          for {
            subList <- Suspend(() ⇒ loop(tl, size - 1))
            acc <- Suspend(() ⇒ loop(tl, size))
            all <- allInserts(hd, subList)
          } yield Mk.combineK(all, acc)
        case _ ⇒ Return(Mk.empty)
      }
    }

    Trampoline.run(loop(seq, size))
  }

  import SeqT._

  def simple[A](seq: Seq[A], size: Int) = seq.combinations(size).flatMap(_.permutations.toList).toStream

  private val list = Stream.from(1).take(5)
  private val i = 3

  private val simp = simple(list, i)
  private val mygen = myGen(list, i)
  private val catsP = catsPerm(list, i)

  private val seqs = List(simp, mygen, catsP)

  compareSeq(seqs.map(_.size))(_ == _).withFilter(!_._1).foreach(println(_))
  compareSeq(seqs) { (a, c) ⇒
    a.forall(x => c.contains(x))
    c.forall(x => a.contains(x))
  } withFilter (!_._1) foreach (println(_))

  println(mygen.size)

  def compareSeq[B](seq: Seq[B])(pred: (B, B) ⇒ Boolean) = {

    seq.headOption.map {
      h ⇒
        seq.foldLeft((true, h)) {
          (acc, curr) ⇒ if (acc._1) pred(curr, acc._2) -> curr else false -> acc._2
        }
    }
  }
}