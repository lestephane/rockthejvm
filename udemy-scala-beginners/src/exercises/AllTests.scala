package exercises

import lectures.part3fp.{CombineTest, CurryTest}

object AllTests extends App {
  private val noargs: Array[String] = Array[String]()
  CombineTest.main(noargs)
  CurryTest.main(noargs)
  FoldTest.main(noargs)
  ForeachTest.main(noargs)
  ForTest.main(noargs)
  MaybeTest.main(noargs)
  MergeTest.main(noargs)
  MyListTest.main(noargs)
  SortTest.main(noargs)
  SplitTest.main(noargs)
  ZipWithTest.main(noargs)
}
