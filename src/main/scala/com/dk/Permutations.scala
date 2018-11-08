package com.dk

object Permutations extends App {

  object Ugly {

    def getAllPermutation(words:Seq[String], length:Int):Option[scala.collection.mutable.ArrayBuffer[Seq[String]]] = {
      def printWordsPermutationRec(res:scala.collection.mutable.ArrayBuffer[Seq[String]],tempRes:Seq[String], words:Seq[String], length:Int):scala.collection.mutable.ArrayBuffer[Seq[String]] = {
        length match{
          case 0 => res += tempRes
          case _ => for (word <- words){
            val t = tempRes :+ word
            printWordsPermutationRec(res,t, words.filter(_!=word), length-1)
          }
        }
        res
      }

      if(words.lengthCompare(length) < 0 || length < 0)
        None
      else {
        val s = scala.collection.mutable.ArrayBuffer[Seq[String]]()
        val f =  Seq[String]()
        Some(printWordsPermutationRec(s,f, words, length))
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

  private def runAll[A](seq: Seq[A], i: Int) = {
    val s = time(simple(seq, i))
    print(s.size + " , ")
    val y = time(perms(seq, i))
    print(y.size + " , ")
    val u = time(Ugly.getAllPermutation(seq.map(_.toString), i).toList.flatten)
    print(u.size + " , ")
    val x = time(s/*unsafePerms(seq, i)*/)
    print(x.size + " , ")

    println()

    if (y.size != x.size || x.size != y.size || y.size != s.size || s.size != u.size)
      println(s"${s.size} - ${x.size} - ${y.size} - ${u.size}")

    val check1 = sSet.zip(ySet).filterNot(e ⇒ e._1 == e._2).mkString("\n")
    val check2 = sSet.zip(xSet).filterNot(e ⇒ e._1 == e._2).mkString("\n")
    val check3 = sSet.zip(xSet).filterNot(e ⇒ e._1 == e._2).mkString("\n")
    if (check1.nonEmpty || check2.nonEmpty || check3.nonEmpty) println(check1 + " - " + check2 + " - " + check3)
  }

  def time[A](block: ⇒ A): A = {
    val pre = System.nanoTime()
    val b = block
    val post = System.nanoTime()
    print(s"${post - pre},")

    b
  }

 // println(Ugly.getAllPermutation((1 to 8).toList.map(_.toString),3))

  for {
    s <- 1 to 15
    j <- 1 to s
  } runAll((1 to s).toList, j)
}

