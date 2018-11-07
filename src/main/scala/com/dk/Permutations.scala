package com.dk

object Permutations extends App {
  def unsafePerms[A](seq: Seq[A], size: Int): Seq[Seq[A]] = {
    val seqSize = seq.size

    def loop(seq: Seq[A], size: Int): Seq[Seq[A]] = {
      def allInserts(x: A, ll: Seq[Seq[A]]): Seq[Seq[A]] = ll match {
        case hd :: tl => inserts(x, hd) ++: allInserts(x, tl)
        case _ => Nil
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
        case hd :: tl =>
          Suspend(() ⇒ allInserts(x, tl))
            .flatMap(all ⇒ Return(inserts(x, hd) ++: all))

        case _ => Return(Nil)
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

  private def runAll[A](seq: Seq[A], i: Int) = {
    val s = time(simple(seq, i))
    print(s.size + " , ")
    val y = time(perms(seq, i))
    print(y.size + " , ")
    val x = time(unsafePerms(seq, i))
    println(x.size)


    val xSet = x.toSet.toList
    val ySet = y.toSet.toList
    val sSet = s.toSet.toList

    if (sSet.size != xSet.size || xSet.size != ySet.size || ySet != sSet)
      println(s"${sSet.size} - ${xSet.size} - ${ySet.size}")

    val check1 = sSet.zip(ySet).filterNot(e ⇒ e._1 == e._2).mkString("\n")
    val check2 = sSet.zip(xSet).filterNot(e ⇒ e._1 == e._2).mkString("\n")
    if (check1.nonEmpty || check2.nonEmpty) println(check1 + "\n----\n" +check2)
  }

  def time[A](block: ⇒ A): A = {
    val pre = System.currentTimeMillis()
    val b = block
    val post = System.currentTimeMillis()
    print(s"${post - pre},")

    b
  }

  for {
    s <- 1 to 8
    j <- 1 to s
  } runAll((1 to s).toList, j)
}

