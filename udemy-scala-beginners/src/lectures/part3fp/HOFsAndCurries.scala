package lectures.part3fp

import lectures.part3fp.HOFsAndCurries.{andThen, compose, fromCurry, toCurry}

object HOFsAndCurries extends App {
  // 2. toCurry(f: (Int, Int)) => (Int => Int => Int)
  def toCurry(f: (Int, Int) => Int): Int => Int => Int =
    x => y => f(x, y)

  // 2. fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) => f(x)(y)

  // 3. compose(f,g) => x => f(g(x))
  def compose(f: Int => Int, g: Int => Int): (Int) => Int = x => f(g(x))

  // 3. andThen(f,g) => x => g(f(x))
  def andThen(f: Int => Int, g: Int => Int): (Int) => Int = x => g(f(x))
}

object CurryTest extends App {
  val curryMult = toCurry(_ * _)
  assert(curryMult(2)(3) == 6)

  val mult = fromCurry(curryMult)
  assert(mult(2, 3) == 6)

  def double(x: Int): Int = x * 2
  def square(x: Int): Int = x * x

  assert(compose(double, square)(2) == 8)
  assert(andThen(double, square)(2) == 16)
}
