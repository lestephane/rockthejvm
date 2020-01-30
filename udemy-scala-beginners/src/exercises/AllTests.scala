package exercises

import lectures.part3fp.CurryTest

object AllTests extends App {
  private val noargs: Array[String] = Array[String]()
  CurryTest.main(noargs)
  FoldTest.main(noargs)
  ForeachTest.main(noargs)
  MergeTest.main(noargs)
  MyListTest.main(noargs)
  SortTest.main(noargs)
  SplitTest.main(noargs)
  ZipWithTest.main(noargs)
}
