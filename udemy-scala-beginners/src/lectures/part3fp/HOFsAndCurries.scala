package lectures.part3fp

import lectures.part3fp.HOFsAndCurries.{andThen, compose, fromCurry, toCurry}

object HOFsAndCurries extends App {
  // 2. toCurry(f: (Int, Int)) => (Int => Int => Int)
  def toCurry[A,B,C](f: (A, B) => C): A => B => C =
    x => y => f(x, y)

  // 2. fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
  def fromCurry[A, B, C](f: A => B => C): (A, B) => C =
    (x, y) => f(x)(y)

  // 3. compose(f,g) => x => f(g(x))
  def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))

  // 3. andThen(f,g) => x => g(f(x))
  def andThen[A,B,C](f: A => B, g: B => C): A => C = x => g(f(x))
}

object CurryTest extends App {
  val curryMult = toCurry((x: Int, y: Int) => x * y)
  assert(curryMult(2)(3) == 6)

  val mult = fromCurry(curryMult)
  assert(mult(2, 3) == 6)

  def double(x: Int): Int = x * 2
  def square(x: Int): Int = x * x

  assert(compose(double, square)(2) == 8)
  assert(andThen(double, square)(2) == 16)
}
