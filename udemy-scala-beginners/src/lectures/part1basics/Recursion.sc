object Recursion extends App {
  def concat(n: Int, s: String) = {
    def concatHelper(n: Int, s: String, acc: String): String =
      if (n == 0) acc
      else concatHelper(n - 1, s, acc + s)

    concatHelper(n, s, "")
  }

  def concatTest(n: Int, s: String, result: String) =
    assert(concat(n, s).equals(result), s"concat($n, $s) should be $result")

  concatTest(0, "0", "")
  concatTest(1, "1", "1")
  concatTest(2, "2", "22")
  concatTest(3, "3", "333")

  def isPrime(n: BigInt): Boolean = {
    def isPrimeUntil(m: BigInt): Boolean = {
      if (m <= 0) false
      else if (m == 1) true
      else if (n % m == 0) false
      else isPrimeUntil(m -1 )
    }

    if (n == 1) true
    else isPrimeUntil(n/2)
  }

  def isPrimeTest(n: BigInt, result: Boolean) =
    assert(isPrime(n).equals(result), s"isPrime($n) == $result")

  isPrimeTest(0, false)
  isPrimeTest(1, true)
  isPrimeTest(2, true)
  isPrimeTest(3, true)
  isPrimeTest(4, false)
  isPrimeTest(5, true)
  isPrimeTest(-5, false)
  //isPrimeTest(2147483647, true)

  def fib(n:BigInt): BigInt = {
    def fibRec(i: BigInt, last: BigInt, beforeLast: BigInt): BigInt =
      if (i >= n) last
      else fibRec(i + 1, last + beforeLast, last)

    if (n == 0 ) 0
    else if (n == 1) 1
    else fibRec(2, 1, 1)
  }

  def fibTest(n: BigInt, result: BigInt) =
    assert(fib(n).equals(result), s"fib($n) == $result (got ${fib(n)} instead)")

  fibTest(0, 0)
  fibTest(1, 1)
  fibTest(2, 1)
  fibTest(3, 2)
  fibTest(4, 3)
  fibTest(5, 5)
  fibTest(6, 8)
  fibTest(7, 13)
}
