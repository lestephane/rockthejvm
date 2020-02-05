package lectures.part3fp

import scala.util.{Random, Try}

object Failures extends App {
  val hostname = "localhost"
  val port = "8080"
  def renderHTML(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) s"<html><title>$url</title></html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())
    def getConnection(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")
    }
  }

  // exercise print the page if
  // - you obtain a connection
  // - you obtain a page from that connection
  for {
    connection <- Try(HttpService.getConnection(hostname, port))
    page <- Try(connection.get("page"))
  } println(page)
}
