package lectures.part2oop

object Exceptions extends App {
  val l = try {
    new Array[Byte](Int.MaxValue)
  } catch {
    case _: OutOfMemoryError => "oom"
  }
  assert(l == "oom")

  val b = try {
    def rec(b: Boolean): Int = {
      if (true) rec(b) + 1
      else 0
    }
    rec(true)
  } catch {
    case _: StackOverflowError => "soe"
  }
  assert(b == "soe")

  object PocketCalculator {
    def add(t1: Int, t2: Int): Int = {
      val result: Double = t1.toDouble + t2.toDouble
      if (result > Int.MaxValue.toDouble) throw new OverflowException()
      else result.toInt
    }

    def mul(x: Int, y: Int): Int = {
      val result: Double = x.toDouble * y.toDouble
      if (result > Int.MaxValue.toDouble) throw new OverflowException()
      else if (result < Int.MinValue.toDouble) throw new UnderflowException()
      else result.toInt
    }

    def div(dividend: Int, divisor: Int): Int = {
      if (divisor == 0) throw new MathCalculationException()
      else dividend / divisor
    }

    def sub(minuend: Int, subtrahend: Int): Int = {
      val result:Double = minuend.toDouble - subtrahend
      if (result < Int.MinValue) throw new UnderflowException()
      else result.toInt
    }
  }

  class OverflowException() extends Throwable
  class UnderflowException() extends Throwable
  class MathCalculationException extends Throwable

  assert(PocketCalculator.add(1, 2) == 3, "1 + 2  == 3")
  assert(PocketCalculator.sub(1, 2) == -1, "1 - 2  == -1")
  assert(PocketCalculator.mul(2, 3) == 6, "2 * 3  == 6")
  assert(PocketCalculator.div(6, 3) == 2, "6 / 3  == 2")

  def thrownBy(fun: => Any) = try {
    fun
  } catch {
    case e: Throwable => e
  }

  assert(thrownBy(PocketCalculator.add(Int.MaxValue, 1)).isInstanceOf[OverflowException])
  assert(thrownBy(PocketCalculator.sub(Int.MinValue, 1)).isInstanceOf[UnderflowException])
  assert(thrownBy(PocketCalculator.mul(Int.MaxValue, 2)).isInstanceOf[OverflowException])
  assert(thrownBy(PocketCalculator.div(Int.MaxValue, 0)).isInstanceOf[MathCalculationException])
}
