package urwerk.io.file

case class Attributes(headers: Map[String, Seq[Attributes.V]]):
  import Attributes.*
end Attributes

object Attributes:
  type V = Boolean | Int | Long | Float | Double | String | Product


end Attributes
