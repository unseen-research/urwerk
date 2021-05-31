package urwerk.io.http

object Header:
  type Val = Boolean | Int | Long | Float | Double | String

  trait ApplyVal[N <: String]:
    type V
    def apply(value: V): Val

  type ApplyValAux[N <: String, V1] = ApplyVal[N] {
    type V = V1
  }

  trait ApplySeq[N <: String]:
    type V
    def apply(value: Seq[V]): Seq[Val]

  type ApplySeqAux[N <: String, V1] = ApplySeq[N] {
    type V = V1
  }

  trait UnapplySeq[N <: String]:
    type V
    type VV
    def unapply(values: Seq[Val]): VV

  type ContentLength = "content-length"

  object ContentLengthHeader:
    val name = valueOf[ContentLength]

  given UnapplySeq[ContentLength] with ApplyVal[ContentLength] with {
    type V = Long
    type VV = Option[Long]
    def unapply(values: Seq[Val]): Option[Long] =
      values.lastOption
        .map{_ match
          case value: Number => value.longValue
          case value => value.toString.toLong }

    def apply(value: V): Val = value
  }

  type Accept = "accept"

  object AcceptHeader:
    val name = valueOf[Accept]

  given UnapplySeq[Accept] with ApplyVal[Accept] with ApplySeq[Accept] with {
    type V = String
    type VV = Seq[String]
    def unapply(values: Seq[Val]): Seq[String] =
      values.map(_.toString)

    def apply(value: V): Val = value
    def apply(values: Seq[V]): Seq[Val] = values
  }

  given AnyUnapplySeq[N <: String]: UnapplySeq[N] with {
    type V = Val
    type VV = Seq[Val]
    def unapply(values: Seq[Val]): Seq[Val] =
      values
  }

end Header

case class Headers(headers: Map[String, Seq[Header.Val]]):
  import Headers.*
  import Header.*

  inline def add[N <: String]: AddOp[N] =
    AddOp[N](this, valueOf[N])

  def add(name: String, value: Val, values: Val*): Headers =
    add(name, value +: values)

  def add(name: String, values: Seq[Val]): Headers =
    val currentValues = headers.getOrElse(name, Seq())
    Headers(headers.updated(name, currentValues ++ values))

  def set(name: String, values: Seq[Val]): Headers =
    Headers(headers.updated(name, values))

  inline def set[N <: String]: SetOp[N] =
    SetOp[N](this, valueOf[N])

  def set(name: String, value: Val, values: Val*): Headers =
    set(name, value +: values)

  def apply(name: String): Seq[Val] =
    headers.getOrElse(name, Seq())

  inline def apply[N <: String](using extr: UnapplySeq[N]): extr.VV =
    val name = valueOf[N]
    extr.unapply(
      apply(name))

object Headers:
  import Header.*

  def apply(headers: (String, Val)*): Headers =
    Headers(
      headers.groupMap((k, _) => k)((_, v) => v))

  class AddOp[N <: String](headers: Headers, name: String):
    def apply[V](value: V)(using applyVal: ApplyValAux[N, V]): Headers =
      headers.add(name, applyVal(value))

    def apply[V](value: V*)(using applySeq: ApplySeqAux[N, V]): Headers =
      headers.add(name, applySeq(value))

  class SetOp[N <: String](headers: Headers, name: String):
    def apply[V](value: V)(using applyVal: ApplyValAux[N, V]): Headers =
      headers.set(name, applyVal(value))

    def apply[V](value: V*)(using applySeq: ApplySeqAux[N, V]): Headers =
      headers.set(name, applySeq(value))
