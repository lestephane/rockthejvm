package lectures.part3fp

object HOFsAndCurries extends App {
  // 2. toCurry(f: (Int, Int)) => (Int => Int => Int)
  def toCurry(f: (Int, Int) => Int): Int => Int => Int =
    (x: Int) => (y: Int) => f(x, y)

  val curryMult = toCurry(_ * _)
  assert(curryMult(2)(3) == 6)

  // 2. fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x: Int, y: Int) => f(x)(y)

  val mult = fromCurry(curryMult)
  assert(mult(2, 3) == 6)

  // support functions for compose() andThen() test cases
  def double(x: Int): Int = x * 2
  def square(x: Int): Int = x * x

  // 3. compose(f,g) => x => f(g(x))
  def compose(f: Int => Int, g: Int => Int): (Int) => Int = x => f(g(x))

  assert(compose(double, square)(2) == 8)

  // 3. andThen(f,g) => x => g(f(x))
  def andThen(f: Int => Int, g: Int => Int): (Int) => Int = x => g(f(x))
  assert(andThen(double, square)(2) == 16)
}
