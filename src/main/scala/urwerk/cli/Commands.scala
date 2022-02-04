package urwerk.cli

object Commands:

  trait Setting

  def apply(setting: Setting, settings: Setting*): Commands = ???

class Commands