package lectures.part3fp

object TuplesAndMaps extends App{
  println(Map("john" -> 1, "JOHN" -> 2).map(pair => pair._1.toLowerCase -> pair._2))
  println(List("a", "ab", "b", "bc").groupBy(_.contains("b")))
  println(Map("a" -> 1, "ab" -> 2, "b" -> 3, "bc" -> 4).groupBy(pair => pair._2 % 2 == 0))
}
