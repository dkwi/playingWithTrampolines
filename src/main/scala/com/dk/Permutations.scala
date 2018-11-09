package com.dk

object Permutations extends App {

  object Ugly {

    def getAllPermutation(words: Seq[String], length: Int): Option[scala.collection.mutable.ArrayBuffer[Seq[String]]] = {
      def printWordsPermutationRec(res: scala.collection.mutable.ArrayBuffer[Seq[String]], tempRes: Seq[String], words: Seq[String], length: Int): scala.collection.mutable.ArrayBuffer[Seq[String]] = {
        length match {
          case 0 => res += tempRes
          case _ => for (word <- words) {
            val t = tempRes :+ word
            printWordsPermutationRec(res, t, words.filter(_ != word), length - 1)
          }
        }
        res
      }

      if (words.lengthCompare(length) < 0 || length < 0)
        None
      else {
        val s = scala.collection.mutable.ArrayBuffer[Seq[String]]()
        val f = Seq[String]()
        Some(printWordsPermutationRec(s, f, words, length))
      }
    }
  }

  def unsafePerms[A](seq: Seq[A], size: Int): Seq[Seq[A]] = {
    val seqSize = seq.size

    def loop(seq: Seq[A], size: Int): Seq[Seq[A]] = {
      def allInserts(x: A, ll: Seq[Seq[A]]): Seq[Seq[A]] = ll match {
        case hd :: tl ⇒ inserts(x, hd) ++: allInserts(x, tl)
        case _ ⇒ Nil
      }

      def inserts(x: A, ll: Seq[A]): Seq[Seq[A]] =
        (0 to ll.length).map { i ⇒ insertOne(i, x, ll) }

      def insertOne(i: Int, x: A, ll: Seq[A]): Seq[A] =
        ll.slice(0, i) ++ (x +: ll.slice(i, ll.length))

      seq match {
        case _ if size > seqSize ⇒ Nil
        case _ :: _ if size == 1 ⇒ seq.map(Seq(_))
        case hd :: tl ⇒ allInserts(hd, loop(tl, size - 1)) ++ loop(tl, size)
        case _ ⇒ Nil
      }
    }

    loop(seq, size)
  }


  def perms[A](seq: Seq[A], size: Int): Seq[Seq[A]] = {
    val seqSize = seq.size

    def loop(seq: Seq[A], size: Int): Trampoline[Seq[Seq[A]]] = {
      def allInserts(x: A, ll: Seq[Seq[A]]): Trampoline[Seq[Seq[A]]] = ll match {
        case hd :: tl ⇒
          Suspend(() ⇒ allInserts(x, tl))
            .flatMap(all ⇒ Return(inserts(x, hd) ++: all))

        case _ ⇒ Return(Nil)
      }

      def inserts(x: A, ll: Seq[A]): Seq[Seq[A]] =
        (0 to ll.length).map { i ⇒ insertOne(i, x, ll) }

      def insertOne(i: Int, x: A, ll: Seq[A]): Seq[A] =
        ll.slice(0, i) ++ (x +: ll.slice(i, ll.length))

      seq match {
        case _ if size > seqSize ⇒ Return(Nil)
        case _ :: _ if size == 1 ⇒ Return(seq.map(Seq(_)))
        case hd :: tl ⇒ Suspend(() ⇒ loop(tl, size - 1))
          .flatMap(subList ⇒ Suspend(() ⇒ loop(tl, size))
            .flatMap(acc ⇒ allInserts(hd, subList)
              .flatMap(all ⇒ Return(all ++ acc))))
        case _ ⇒ Return(Nil)
      }
    }

    Trampoline.run(loop(seq, size))
  }

  def simple[A](seq: Seq[A], size: Int): Seq[Seq[A]] = seq.combinations(size).flatMap(_.permutations).toList

  private def runAll[A](seq: Seq[A], i: Int)(ops: ((Seq[A], Int) ⇒ Seq[Seq[A]])*): Unit = {
    val res = ops map { op ⇒ time(op(seq, i)) }

    val meta = res map { case (t, s) ⇒ t -> s.size }

    println(meta.map(a ⇒ s"${a._1}, ${a._2}").mkString(" , "))

    compareSeq(meta)((a, c) ⇒ c._2 == a._2).withFilter(!_._1).foreach(println(_))

    compareSeq(res) { (a, c) ⇒
      a._2.forall(x => c._2.contains(x))
      c._2.forall(x => a._2.contains(x))
    } withFilter (!_._1) foreach (println(_))

  }


  def compareSeq[B](seq: Seq[B])(pred: (B, B) ⇒ Boolean) = {

    seq.headOption.map {
      h ⇒
        seq.foldLeft((true, h)) {
          (acc, curr) ⇒ if (acc._1) pred(curr, acc._2) -> curr else false -> acc._2
        }
    }
  }

  def time[A](block: ⇒ A): (Long, A) = {
    val pre = System.nanoTime()
    val b = block
    val post = System.nanoTime()

    (post - pre, b)
  }

  // println(Ugly.getAllPermutation((1 to 8).toList.map(_.toString),3))

  for {
    s <- 1 to 15
    j <- 1 to s
  } runAll((1 to s).toList, j)(
    simple,
    perms,
    (s, i) ⇒ Ugly.getAllPermutation(s.map(_.toString), i).toList.flatten.map(_.map(_.toInt)),
    simple //unsafePerms(seq, i)
  )
}

