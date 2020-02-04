package lectures.part3fp

import scala.util.Random

object Options extends App {
  val config: Map[String,String] = Map(
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    val random =new Random(System.nanoTime())
    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  // try to establish a connection, if so - print the connect method
  println(for {
    host <- config.get("host")
    port <- config.get("port")
  } yield Connection(host, port).fold("not connected")(_.connect))
}
