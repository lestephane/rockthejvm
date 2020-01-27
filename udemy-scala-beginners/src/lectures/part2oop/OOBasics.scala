package lectures.part2oop

object OOBasics extends App {
  println("hello")
}

class Writer(firstname: String, surname: String, val yearOfBirth: Int) {
  def fullName = s"$firstname $surname"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge = yearOfRelease - author.yearOfBirth
  def isWrittenBy(author: Writer) = author.fullName == author.fullName
  def copy(year: Int) = new Novel(name, year, author)
}

class Counter(i: Int = 0) {
  def currentCount = i
  def increment(amount: Int = 1) = new Counter(i + amount)
  def decrement(amount: Int = 1) = new Counter(i - amount)
}

object test extends App {

  println("1" == "1")

}
