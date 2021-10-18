package urwerk.io

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.channels.Channels

import scala.annotation.tailrec

import urwerk.source.Source
import urwerk.source.Sink
import java.io.OutputStream

object Streams:
  val DefaultBufferSize: Int = 4096 * 2

  extension [A <: OutputStream](outputStream: A)
    def write(bytes: ByteString): A =
      outputStream.write(bytes.array, bytes.offset, bytes.length)
      outputStream

  extension (inputStream: InputStream)
    def toSource: Source[ByteString] =
      toSource(Streams.DefaultBufferSize)

    def toSource(blockSize: Int): Source[ByteString] = readBytes(inputStream, blockSize)

private def readBytes(inputStream: InputStream, blockSize: Int): Source[ByteString] =
  Source.using(Channels.newChannel(inputStream), _.close){channel =>
    Source.create[ByteString]{sink =>
      val buffer: ByteBuffer = ByteBuffer.allocate(blockSize)
      readBytes(channel, buffer, sink, blockSize)
    }
  }

@tailrec
private def readBytes(
    channel: ReadableByteChannel,
    buffer: ByteBuffer,
    sink: Sink[ByteString],
    blockSize: Int): Unit =
  val size = channel.read(buffer)

  if size < 0 then
    sink.complete()
  else
    buffer.flip()
    if (buffer.limit() > 0) {
      sink.next(ByteString.from(buffer))
    }
    readBytes(channel, buffer.clear(), sink, blockSize)
