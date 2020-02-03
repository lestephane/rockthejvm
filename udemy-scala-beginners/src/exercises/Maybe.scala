package exercises

abstract class Maybe[+A]{
  def isEmpty: Boolean
  def map[B >: A](f: A => B): Maybe[B]
  def flatMap[B >: A](f: A => Maybe[B]): Maybe[B]
}

case object None extends Maybe[Nothing] {
  override def isEmpty: Boolean = true
  override def map[B >: Nothing](f: Nothing => B): Maybe[B] = None
  override def flatMap[B >: Nothing](f: Nothing => Maybe[B]): Maybe[B] = None
}

case class Some[A](value: A) extends Maybe[A] {
  override def isEmpty: Boolean = false
  override def map[B >: A](f: A => B): Maybe[B] = Some(f(value))
  override def flatMap[B >: A](f: A => Maybe[B]): Maybe[B] = f(value)
}

object MaybeTest extends App {
  assert(None.map(x => ???) == None)
  assert(Some(1).map(x => 2) == Some(2))

  assert(None.flatMap(x => ???) == None)
  assert(Some(1).flatMap(x => Some("" + x + x)) == Some("11"))
}
