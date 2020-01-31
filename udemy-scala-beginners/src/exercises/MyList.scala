package exercises

import exercises.MyListTest.{listOf}

import scala.collection.mutable.ListBuffer

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](n: B): MyList[B] = Cons[B](n, this)
  def elementsToString(): String
  def ++[B >: A](other: MyList[B]): MyList[B]
  def map[B](trans: A => B): MyList[B]
  def flatMap[B](trans: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def foreach(f: A => Unit)
  def sort[B >: A](cmp: (B, B) => Int): MyList[B]
  def zipWith[B,C](ys: MyList[B], f: (A, B) => C): MyList[C]
  def fold[B >: A](start: B)(operator: (B, B) => B): B
  override def toString: String = '[' + elementsToString() + ']'
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
  override def foreach(f: Nothing => Unit): Unit = ()
  override def sort[B >: Nothing](cmp: (B, B) => Int): MyList[B] = Empty
  override def zipWith[B, C](ys: MyList[B], f: (Nothing, B) => C): MyList[C] = {
    assert(ys.isEmpty, "Empty.zipWith() attempted with another non-empty list"); Empty
  }
  override def fold[B >: Nothing](start: B)(operator: (B, B) => B): B = start
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

  override def map[B](trans: A => B): MyList[B] =
    tail.map(trans).add(trans(head))

  override def flatMap[B](trans: A => MyList[B]): MyList[B] =
    trans(h) ++ tail.flatMap(trans)

  override def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h))
      Cons(h, tail.filter(predicate))
    else
      tail.filter(predicate)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def sort[B >:A](cmp: (B, B) => Int): MyList[B] = MergeSortOps.mergeSort(this, cmp)

  override def zipWith[B,C](ys: MyList[B], zip: (A, B) => C): MyList[C] =
    if (ys.isEmpty) Empty
    else Cons(zip(head, ys.head), t.zipWith(ys.tail, zip))

  override def fold[B >: A](start: B)(operator: (B, B) => B): B =
    t.fold(operator(start, h))(operator)
}

object MergeSortOps {
  def mergeSort[A](xs: MyList[A], comparator: (A, A) => Int): MyList[A] = {
    if (xs == Empty || xs.tail == Empty) xs
    else {
      val (left, right) = splitList(xs)
      val leftSorted = mergeSort(left, comparator)
      val rightSorted = mergeSort(right, comparator)
      merge(leftSorted, rightSorted, comparator)
    }
  }

  // split() function needed by sort()
  def splitList[A](list: MyList[A]): (MyList[A], MyList[A]) = {
    def splitListRec(cur: MyList[A], acc1: MyList[A], acc2: MyList[A]): (MyList[A], MyList[A]) = {
      if (cur == Empty)
        if (acc1 == Empty) (acc2, acc1)
        else (acc1, acc2)
      else splitListRec(cur.tail, acc2, acc1.add(cur.head))
    }
    splitListRec(list, Empty, Empty)
  }

  def merge[A](xs: MyList[A], ys: MyList[A], comparator: (A, A) => Int): MyList[A] = {
    def rmerge(acc: MyList[A], left: MyList[A], right: MyList[A]): MyList[A] =
      if (left == Empty) acc ++ right
      else if (right == Empty) acc ++ left
      else {
        val leftBeforeRight = comparator(left.head, right.head) <= 0
        leftBeforeRight match {
          case true => rmerge(acc ++ Cons(left.head, Empty), left.tail, right)
          case false => rmerge(acc ++ Cons(right.head, Empty), left, right.tail)
        }
      }
    rmerge(Empty, xs, ys)
  }
}

object MyListTest extends App {
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
  assert({
    val list = new ListBuffer[Int](); Empty.foreach(_ => ???); list.length
  } == 0)
  assert({
    val list = new ListBuffer[Int]();
    listOf(1).foreach(list.+=);
    list.toString()
  } == "ListBuffer(1)")
}

object ForeachTest extends App {
  def foreachTest(xs: Int*) = {
    val list = ListBuffer[Int]()
    listOf(xs: _*).foreach(list.+=)
    list.toString()
  }

  val testForeachOnMany = () => foreachTest(1, 2)
  assert(testForeachOnMany() == "ListBuffer(1, 2)", testForeachOnMany)
}

object SplitTest extends App {
  // split() tests
  assert(MergeSortOps.splitList(Empty) == (Empty, Empty))
  assert(MergeSortOps.splitList(listOf(1)) == (listOf(1), Empty))
  assert(MergeSortOps.splitList(listOf(1, 2)) == (listOf(1), listOf(2)))
  assert(MergeSortOps.splitList(listOf(1, 2, 3)) == (listOf(2), listOf(3,1)))
}

// merge() tests
object MergeTest extends App {
  def mergeTestAsc[A](xs: MyList[A], ys: MyList[A])(implicit orderer: A => Ordered[A]) =
    MergeSortOps.merge[A](xs, ys, _.compareTo(_))

  assert(mergeTestAsc[Int](Empty, Empty) == Empty)
  assert(mergeTestAsc[Int](listOf(1), Empty) == listOf(1))
  assert(mergeTestAsc[Int](Empty, listOf(1)) == listOf(1))
  assert(mergeTestAsc[Int](listOf(1), listOf(2)) == listOf(1, 2))
  assert(mergeTestAsc[Int](listOf(2), listOf(1)) == listOf(1, 2))
  assert(mergeTestAsc[Int](listOf(1,2), listOf(1)) == listOf(1, 1, 2))
  assert(mergeTestAsc[Int](listOf(1), listOf(1, 2)) == listOf(1, 1, 2))
  assert(mergeTestAsc[Int](listOf(2), listOf(1, 3)) == listOf(1, 2, 3))
}

// sort() tests
object SortTest extends App {
  def sortTest[A](xs: A*)(implicit orderer: A => Ordered[A]): MyList[A] = {
    listOf(xs: _*).sort(_.compareTo(_))
  }

  def sortTestDesc[A](xs: A*)(implicit orderer: A => Ordered[A]): MyList[A] = {
    listOf(xs: _*).sort((x, y) => y.compareTo(x))
  }

  assert(Empty.sort((_: Any, _: Any) => 0) == Empty)
  assert(sortTest[Int]() == Empty)
  assert(sortTest[Int](1) == listOf(1))
  assert(sortTest[Int](2, 1) == listOf(1, 2))
  assert(sortTest[Int](2, 1, 3, 1) == listOf(1, 1, 2, 3))

  // $ seq 1 10 | shuf | tr '\n' ','
  private val sortedAsc: MyList[Int] = sortTest[Int](2,8,3,7,4,9,5,6,10,1)
  assert(sortedAsc == listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), sortedAsc)

  private val sortedDesc: MyList[Int] = sortTestDesc[Int](2,8,3,7,4,9,5,6,10,1)
  assert(sortedDesc == listOf(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), sortedDesc)
}

object ZipWithTest extends App {
  assert(listOf(1, 2, 3).zipWith(listOf(4, 5, 6), (x: Int, y: Int) => x * y) == listOf(4, 10, 18))
  assert(listOf(1, 2, 3).zipWith(listOf('a', 'b', 'c'), (x: Int, y: Char) => y.toString * x) == listOf("a", "bb", "ccc"))
}

object FoldTest extends App {
  assert(listOf(1, 2, 3).fold(0)((x: Int, y: Int) => x + y) == 6)
  assert(listOf(3, 3).fold(1)((x: Int, y: Int) => x * y) == 9)
}

object ForTest extends App {
  val numbers = listOf(1, 2)
  val chars = listOf('a', 'b')
  val result = for {
    i <- chars
    j <- numbers
  } yield "" + i + j
  assert(result == listOf("a1", "a2", "b1", "b2"), result)
}
