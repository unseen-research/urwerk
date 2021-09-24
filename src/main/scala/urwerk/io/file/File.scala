package urwerk.io.file

import urwerk.io
import urwerk.io.{ByteString}
import urwerk.source.{Singleton, Sink, Source}

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, ReadableByteChannel}
import java.nio.charset.Charset
import java.nio.file.attribute.{BasicFileAttributeView, BasicFileAttributes}
import java.nio.file.{Files, Paths, StandardOpenOption}

import scala.annotation.tailrec
import scala.io.Codec
import scala.jdk.CollectionConverters.given
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicBoolean

val Cwd: io.Path = Path("")
  .toAbsolutePath.toPath

val Root = io.Path("/")

trait ReadOptions:
  def chunkSize: Int

given ReadOptions with {
  val chunkSize = 1024
}

trait GetAttributes[A]:
  def attributesOf(path: io.Path): A

given GetAttributes[BasicFileAttributes] with {
  def attributesOf(path: io.Path): BasicFileAttributes =
    Files.getFileAttributeView(Path(path), classOf[BasicFileAttributeView])
      .readAttributes()
}

trait PathOps:
  extension (path: io.Path)
    def bytes(using options: ReadOptions): Source[ByteString] =
      readBytes(path, options)

    def attributes[A](using getOp: GetAttributes[A]): Singleton[A] =
      Singleton.defer(
        Singleton(getOp.attributesOf(path)))

    def strings(using codec: Codec, options: ReadOptions): Source[String] =
      bytes.map(_.mkString)

    def isFile: Boolean = Files.isRegularFile(Path(path))

    def isDirectory: Boolean = Files.isDirectory(Path(path))

    def list: Source[io.Path] = Source.create[io.Path]{sink =>
      val stream = Files.list(Path(path))
        .onClose(() => onDirectoryStreamClose(path))

      val iterator = stream.iterator.asScala
      sink.onDispose(stream.close())
      sink.onRequest{requested =>

        var remaining = requested

        while remaining > 0 && iterator.hasNext do
          remaining -= 1
          sink.next(
            iterator.next().toPath)

        if !iterator.hasNext then
          sink.complete()
          stream.close()
      }
    }

    def directories: Source[io.Path] =
      list.filter(_.isDirectory)

    def files: Source[io.Path] =
      list.filter(_.isFile)

  private def readBytes(path: io.Path, options: ReadOptions): Source[ByteString] =
    Source.create[ByteString]{sink =>
      val fileChan = FileChannel.open(Path(path), StandardOpenOption.READ)
      sink.onRequest(requestCount =>
        readBytes(fileChan, requestCount, sink, options))
        .onDispose(
          fileChan.close())
    }

  @tailrec
  private def readBytes(
      channel: ReadableByteChannel,
      requestCount: Long,
      sink: Sink[ByteString],
      options: ReadOptions): Unit =
    if channel.isOpen && requestCount > 0 then
      val buffer: ByteBuffer = ByteBuffer.allocate(options.chunkSize)
      val size = readChannel(channel, buffer)
      if size < 0 then {
        sink.complete()
        ()
      } else
        buffer.flip()
        if (buffer.limit() > 0) {
          sink.next(ByteString.from(buffer))
        }
        readBytes(channel, requestCount - 1, sink, options)
    else
      ()

  private[file] def onDirectoryStreamClose(path: io.Path): Unit = {}

  private[file] def readChannel(channel: ReadableByteChannel, buffer: ByteBuffer): Int =
    channel.read(buffer)

given PathOps with {}

extension (source: Source[io.Path])
  def zipWithAttributes[A](using getOp: GetAttributes[A]): Source[(io.Path, A)] =
    source.map(path => (path, getOp.attributesOf(path)))

//////////////////
import java.nio.channels.AsynchronousFileChannel
import java.nio.channels.CompletionHandler
import java.nio.file.OpenOption

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.given

import urwerk.concurrent.given

type Path = java.nio.file.Path

object Path:
  def apply(element: String, elements: String*): Path =
    Paths.get(element, elements*)

  def apply(path: io.Path): Path =
    apply(path.toString)

trait PathExtensions:
  extension (path: Path)
    def toPath: io.Path = io.Path(path.toString)

given PathExtensions = new PathExtensions {}

trait File:
  extension (file: Path)(using ec: ExecutionContext)
    def createByteSource(): Source[ByteString] =
      read(file)

    def createByteSource(blockSize: Int): Source[ByteString] =
      read(file, blockSize)

object File extends File

given File = File

private def read(path: Path)(using ec: ExecutionContext): Source[ByteString] =
  val blockSize = Files.getFileStore(path).getBlockSize.toInt
  read(path, blockSize)

private def read(path: Path, blockSize: Int)(using ec: ExecutionContext): Source[ByteString] =
  Source.using[ByteString, AsynchronousFileChannel](
      AsynchronousFileChannel.open(path, Set(StandardOpenOption.READ).asJava, ec.toExecutorService),
      channel => channel.close())
    {channel =>
      Source.create{sink =>
        val buffer = ByteBuffer.allocate(blockSize.toInt)
        channel.read(buffer, 0, buffer, ReadCompletionHandler(channel, sink, 0, blockSize.toInt))
      }
    }

private class ReadCompletionHandler(channel: AsynchronousFileChannel, sink: Sink[ByteString], val position: Long, blockSize: Int) extends CompletionHandler[Integer, ByteBuffer]:
  def completed(readCount: Integer, buffer: ByteBuffer): Unit =
    if readCount >= 0 then
      buffer.flip()
      if buffer.limit() > 0 then
        sink.next(ByteString.from(buffer))
      val nextPos = position + readCount
      channel.read(buffer, nextPos, buffer.clear(),
        ReadCompletionHandler(channel, sink, nextPos, blockSize))
    else
      sink.complete();

  def failed(error: Throwable, buffer: ByteBuffer): Unit =
    sink.error(error)
