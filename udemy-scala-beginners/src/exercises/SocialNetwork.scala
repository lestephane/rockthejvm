package exercises

class SocialNetwork {
  var root: Map[String,Set[String]] = Map().withDefault(name => Set())

  def add(name: String) = {
    root = root + (name -> root(name))
    this
  }

  def remove(name: String) = {
    root = (root.mapValues((friends: Set[String]) => friends - name) - name).withDefault(_ => Set())
    this
  }

  def friend(name1: String, name2: String) = {
    root = root + (name1 -> (root(name1) + name2)) + (name2 -> (root(name2) + name1))
    this
  }

  def unfriend(name1: String, name2: String): Unit = {
    root = root + (name1 -> (root(name1) - name2)) + (name2 -> (root(name2) - name1))
  }

  def friendCount(name: String): Int = root(name).size

  def mostFriended(): String = root.maxBy((person: (String, Set[String])) => person._2.size)._1

  def notFriendedCount(): Int = root.count((person: (String, Set[String])) => person._2.isEmpty)

  def connected(start: String, end: String): Boolean = {
    def connectedRec(friends: Set[String], seen: Set[String]): Boolean =
      if (friends.isEmpty) false
      else if (friends.contains(end)) true
      else connectedRec(friends.flatMap(root(_)) -- seen, seen ++ friends)

    connectedRec(root(start), Set())
  }

  override def toString: String = root.toString()
}

case class Person(name: String)

object SocialNetworkTest extends App {
  val net = new SocialNetwork
  net.add("John")
  assert(net.toString == "Map(John -> Set())")

  net.remove("John")
  assert(net.toString == "Map()")

  net.add("John").add("Jane")
  net.friend("John", "Jane")
  assert(net.toString == "Map(John -> Set(Jane), Jane -> Set(John))")

  net.unfriend("John", "Jane")
  assert(net.toString == "Map(John -> Set(), Jane -> Set())")

  assert(net.friendCount("John") == 0)
  assert(net.friend("Jane", "John").friendCount("John") == 1)

  net.add("Joe").friend("Joe", "Jane")
  assert(net.mostFriended() == "Jane")

  assert(net.notFriendedCount() == 0)

  net.unfriend("Joe", "Jane")
  assert(net.notFriendedCount() == 1)

  assert(!net.connected("Joe", "Jane"))
  assert(net.connected("John", "Jane"))

  assert(net.friend("Jane", "Jean").connected("John", "Jean"))

  // Special case: cyclic indirect connection
  // John -> Jane -> Jean -> John
  // Joe
  assert(!net.connected("John", "Joe"))

  assert(!net.remove("John").connected("Jane", "John"))
}
