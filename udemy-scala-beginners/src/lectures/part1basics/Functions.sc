object Functions extends App {
  def greeting(name: String, age: Int) = s"Hi, my name is $name and I am $age years old"
  println(greeting("Stephane", 42))

  def fac(n: Int):Int = {
    if (n == 1) 1
    else n * fac(n-1)
  }

  def factest(n: Int) = println(s"fac($n):${fac(n)}")
  factest(1)
  factest(2)
  factest(3)
  factest(4)

  def fib(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib(n - 1) + fib(n-2)
  }

  def fibtest(n: Int) = println(s"fib($n):${fib(n)}")
  fibtest(0)
  fibtest(1)
  fibtest(2)
  fibtest(3)
  fibtest(4)
  fibtest(5)
  fibtest(6)

  def prime(n: Int): Boolean = {
    def primeUntil(t: Int): Boolean = t match {
      case 0 => false
      case 1 => true
      case _ => n % t != 0 && primeUntil(t - 1)
    }
    n match {
      case n if n < 0 => false
      case 1 => true
      case _ => primeUntil(n / 2)
    }
  }

  def say(s: String): String = s"say($s)"
  assert(!prime(0), say("0 is not a prime"))
  assert(prime(1), say("1 is a prime"))
  assert(prime(2), say("2 is a prime"))
  assert(prime(3), say("3 is a prime"))
  assert(!prime(4), say("4 is not a prime"))
  assert(prime(5), say("5 is a prime"))
  assert(!prime(-5), say("-5 is not a prime"))
  //assert(prime(2147483647), say("2147483647 is a prime"))



  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t-1)

    isPrimeUntil(n / 2)
  }

  println(isPrime(0))

}
