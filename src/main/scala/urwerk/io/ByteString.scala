package urwerk.io

import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.collection.immutable.{IndexedSeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.{IterableOnce, SpecificIterableFactory, mutable}
import scala.io.Codec

object ByteString extends SpecificIterableFactory[Byte, ByteString] {

  def apply(array: Array[Byte]): ByteString = {
    val size = array.size
    val inner = Array.ofDim[Byte](size)
    array.copyToArray(inner)
    new ByteString(inner, 0, size)
  }

  override def apply(bytes: Byte*): ByteString = apply(bytes)

  def apply(bytes: IterableOnce[Byte]): ByteString = {
    ByteString(Array.from(bytes))
  }

  val empty: ByteString = ByteString()

  def newBuilder: mutable.Builder[Byte, ByteString] = mutable.ArrayBuffer.newBuilder[Byte].mapResult(fromSeq)

  def fromInt(byte: Int, bytes: Int*): ByteString = {
    val array = new Array[Byte](bytes.length + 1)
    array(0) = byte.toByte
    val iterator = bytes.iterator
    var i = 1
    while (iterator.hasNext) {
      array(i) = iterator.next().toByte; i += 1
    }
    apply(array)
  }

  def from(string: String): ByteString = {
    ByteString(string.getBytes)
  }

  def from(string: String, encoding: Charset): ByteString = {
    ByteString(string.getBytes)
  }

  def from(buffer: ByteBuffer): ByteString = {
    if (buffer.limit() < 1) {
      empty
    }
    else {
      val array = new Array[Byte](buffer.limit)
      buffer.get(0, array)
      ByteString(array)
    }
  }

  def fromRemaining(buffer: ByteBuffer): ByteString = {
    if (buffer.remaining < 1) {
      empty
    }
    else {
      val array = new Array[Byte](buffer.remaining)
      buffer.get(array)
      ByteString(array)
    }
  }

  def fromSeq(buf: collection.Seq[Byte]): ByteString = {
    ByteString(buf.toArray)
  }

  override def fromSpecific(it: IterableOnce[Byte]): ByteString = {
    it match {
      case seq: collection.Seq[Byte] => fromSeq(seq)
      case _ => fromSeq(mutable.ArrayBuffer.from(it))
    }
  }

  def unsafeWrap(array: Array[Byte]): ByteString = {
    new ByteString(array, 0, array.size)
  }

  def unsafeWrap(array: Array[Byte], offset: Int, length: Int): ByteString = {
    new ByteString(array, offset, length)
  }

  def unsafeWrap(buffer: ByteBuffer): ByteString = {
    if !buffer.hasArray then
      throw new UnsupportedOperationException("The given buffer has no array that could be wrapped")

    unsafeWrap(buffer.array(), 0, buffer.limit)
  }
}

class ByteString (private[io] val array: Array[Byte], private[io] val offset: Int, val length: Int) extends IndexedSeq[Byte],
  IndexedSeqOps[Byte, IndexedSeq, ByteString],
  StrictOptimizedSeqOps[Byte, IndexedSeq, ByteString] {

  verifyIndices()

  override protected def fromSpecific(coll: IterableOnce[Byte]): ByteString = ByteString.fromSpecific(coll)

  override protected def newSpecificBuilder: mutable.Builder[Byte, ByteString] = ByteString.newBuilder

  override def empty: ByteString = ByteString.empty

  override def apply(i: Int): Byte = {
    array(offset+i)
  }

  def concat(suffix: IterableOnce[Byte]): ByteString = appendedAll(suffix)

  def ++(suffix: IterableOnce[Byte]): ByteString = appendedAll(suffix)

  def :+(byte: Byte): ByteString = appended(byte)

  def +:(byte: Byte): ByteString = prepended(byte)

  def ++:(prefix: IterableOnce[Byte]): ByteString = prependedAll(prefix)

  def appendedAll(suffix: IterableOnce[Byte]): ByteString = {
    ByteString(array.concat(suffix))
  }

  def appended(byte: Byte): ByteString = {
    ByteString(array.appended(byte))
  }

  def prepended(byte: Byte): ByteString = {
    ByteString(array.prepended(byte))
  }

  def prependedAll(prefix: IterableOnce[Byte]): ByteString = {
    ByteString(array.prependedAll(prefix))
  }

  private def verifyIndices(): Unit = {
    val aSize = array.size
    if(offset<0) {
      throw new IllegalArgumentException(s"Offset must be positive: offset=$offset")
    }
    if(length<0) {
      throw new IllegalArgumentException(s"Length must be positive: length=$length")
    }
    if(offset > aSize){
      throw new IndexOutOfBoundsException(s"Offset exceeded upper bound: size=$aSize, offset=$offset")
    }
    if(offset + length > aSize){
      throw new IndexOutOfBoundsException(s"Length exceeded upper bound: size=$aSize, offset=$offset, length=$length")
    }
  }

  def toString(using codec: Codec): String =
    new String(array, offset, length, codec.charSet)

  override def toString: String =
    new String(array, offset, length)
}

