package lectures.part3fp

import lectures.part3fp.MapFlatmapFilterFor.combine

object MapFlatmapFilterFor {
  def combine[A, B](xs: List[A], ys: List[B]): List[String] =
    xs.flatMap(x => ys.map(x.toString + _.toString))

}

object CombineTest extends App {
  // print all the combinations between two lists
  val numbers = List(1, 2, 3, 4)
  val chars = List('a', 'b', 'c', 'd')
  assert(combine(List('a'), List(1)) == List("a1"))
  assert(combine(List('a', 'b'), List(1, 2)) == List("a1", "a2", "b1", "b2"))
}

