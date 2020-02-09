package filesystem

import filesystem.files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {
  def show: Unit =
    print(State.SHELL_TOKEN)
}

object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = "") =
    new State(root, wd, output)
}
