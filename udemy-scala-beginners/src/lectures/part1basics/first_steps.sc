def isPrime(n: Int): Boolean = {
  def isPrimeUntil(t: Int): Boolean =
    if (t <= 1) true
    else n % t != 0 && isPrimeUntil(t-1)

  isPrimeUntil(n / 2)
}

println(isPrime(1))
