package filesystem

import java.util.Scanner

object FileSystem extends App {
  val scanner = new Scanner(System.in)
  while (true) {
    print("$ ")
    println(scanner.nextLine())
  }
}
