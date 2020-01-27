package exercises

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MyListWork extends App {
  abstract class MyList[+A] {
    def head: A
    def tail: MyList[A]
    def isEmpty: Boolean
    def add[B >: A](n: B): MyList[B] = Cons[B](n, this)
    def elementsToString(): String
    override def toString: String = '[' + elementsToString() + ']'

    def map[B](trans: A => B): MyList[B]
    def flatMap[B](trans: A => MyList[B]): MyList[B]
    def filter(predicate: A => Boolean): MyList[A]
    def foreach(f: A => Unit)

    def ++[B >: A](other: MyList[B]): MyList[B]
  }

  case object Empty extends MyList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException
    override def tail: MyList[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true
    override def elementsToString(): String = ""
    override def ++[B >: Nothing](other: MyList[B]): MyList[B] = other
    override def map[B](trans: Nothing => B): MyList[B] = Empty
    override def flatMap[B](trans: Nothing => MyList[B]): MyList[B] = Empty
    override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty
    override def foreach(f: Nothing => Unit): Unit = {}
  }

  case class Cons[A] private (h: A, t: MyList[A]) extends MyList[A] {
    override def head: A = h
    override def tail: MyList[A] = t
    override def isEmpty: Boolean = false

    override def elementsToString(): String = {
      val headString = h.toString
      val tailString = tail.elementsToString()
      if (tailString isEmpty) headString
      else headString + ", " + tailString
    }

    override def ++[B >: A](other: MyList[B]): MyList[B] = copy(t = tail ++ other)

    override def foreach(f: A => Unit): Unit = {
      filter(x => { f(x); false } )
    }

    override def map[B](trans: A => B): MyList[B] =
      tail.map(trans).add(trans(head))

    override def flatMap[B](trans: A => MyList[B]): MyList[B] =
      trans(h) ++ tail.flatMap(trans)

    override def filter(predicate: A => Boolean): MyList[A] =
      if (predicate(h))
        Cons(h, tail.filter(predicate))
      else
        tail.filter(predicate)
  }

  def listOf[A](xs: A*): MyList[A] = xs.foldRight[MyList[A]](Empty) {
    (n, l) => Cons(n, l)
  }

  def assertListEquals[A](s: String, xs: A*) = {
    val ls = listOf(xs: _*).toString
    assert(ls == s, s"""input:${xs.mkString("[", ",", "]")} expected:"$s" actual:"$ls"""")
  }

  assertListEquals("[]")
  assertListEquals("[1]", 1)
  assertListEquals("[1, 2]", 1, 2)
  assertListEquals("[1, 2, 3]", 1, 2, 3)

  def assertListEquals[A](expected: String, xs: MyList[A]) = {
    val actual = xs.toString
    assert(expected == actual, s"""expected:"$expected" actual:"$actual"""")
  }

  assertListEquals("[0, 2]", listOf(0, 1, 2).filter(_ % 2 == 0))

  assertListEquals("[]", listOf[Int]().map(_ * 2))
  assertListEquals("[2]", listOf(1).map(_ * 2))
  assertListEquals("[2, 4]", listOf(1, 2).map(_ * 2))

  def nList: Int => MyList[Int] = (n: Int) => {
    def mkList(i: Int, acc: MyList[Int]): MyList[Int] =
      if (i == 0) acc
      else mkList(i - 1, Cons(n, acc))

    mkList(n, Empty)
  }

  assertListEquals("[]", listOf[Int]().flatMap(nList))
  assertListEquals("[1]", listOf(1).flatMap(nList))
  assertListEquals("[2, 2]", listOf(2).flatMap(nList))
  assertListEquals("[1, 2, 2, 3, 3, 3]", listOf(1, 2, 3).flatMap(nList))

  assert(listOf(1, 2, 3) == listOf(1, 2, 3))
  assert(listOf(0, 2, 3) != listOf(1, 2, 3))

  // foreach method A => Unit
  assert({ val list = new ListBuffer[Int](); Empty.foreach(list.append); list.length } == 0)
  assert({ val list = new ListBuffer[Int](); listOf(1).foreach(list.+=); list.toString() } == "ListBuffer(1)")

  def fixture = () => {
    println("hello")
    val list = ListBuffer[Int](); listOf(1, 2).foreach(list.+=); list.toString()
  }
  assert(fixture() == "ListBuffer(1, 2)", fixture())
}
