package lectures.part3fp

object WhatsAFunction extends App {
  val concat = new ((String, String) => String) {
    override def apply(v1: String, v2: String): String = v1 + v2
  }

  val makeMultiplier = new (Int => Int => Int) {
    override def apply(x: Int): Int => Int = (n: Int) => x * n
  }

  assert(makeMultiplier(3)(2) == 6)

  val niceAdder: (Int, Int) => Int = _ + _
}
