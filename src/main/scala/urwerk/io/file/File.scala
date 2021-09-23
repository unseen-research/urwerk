package urwerk.io.file

import urwerk.io.{ByteString, Path}
import urwerk.source.{Singleton, Sink, Source}

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, ReadableByteChannel}
import java.nio.charset.Charset
import java.nio.file.attribute.{BasicFileAttributeView, BasicFileAttributes}
import java.nio.file.{Files, Path => JNFPath, Paths, StandardOpenOption}

import scala.annotation.tailrec
import scala.io.Codec
import scala.jdk.CollectionConverters.given
import scala.language.implicitConversions
import java.nio.channels.CompletionHandler



val Cwd: Path = Paths.get("")
  .toAbsolutePath

val Root = Path("/")

trait ReadOptions:
  def chunkSize: Int

given ReadOptions with {
  val chunkSize = 1024
}

trait GetAttributes[A]:
  def attributesOf(path: Path): A

given GetAttributes[BasicFileAttributes] with {
  def attributesOf(path: Path): BasicFileAttributes =
    Files.getFileAttributeView(path, classOf[BasicFileAttributeView])
      .readAttributes()
}

trait PathOps:
  extension (path: Path)
    def bytes(using options: ReadOptions): Source[ByteString] =
      readBytes(path, options)

    def attributes[A](using getOp: GetAttributes[A]): Singleton[A] =
      Singleton.defer(
        Singleton(getOp.attributesOf(path)))

    def strings(using codec: Codec, options: ReadOptions): Source[String] =
      bytes.map(_.mkString)

    def isFile: Boolean = Files.isRegularFile(path)

    def isDirectory: Boolean = Files.isDirectory(path)

    def list: Source[Path] = Source.create[Path]{sink =>
      val stream = Files.list(path)
        .onClose(() => onDirectoryStreamClose(path))

      val iterator = stream.iterator.asScala
      sink.onDispose(stream.close())
      sink.onRequest{requested =>

        var remaining = requested

        while remaining > 0 && iterator.hasNext do
          remaining -= 1
          sink.next(
            iterator.next())

        if !iterator.hasNext then
          sink.complete()
          stream.close()
      }
    }

    def directories: Source[Path] =
      list.filter(_.isDirectory)

    def files: Source[Path] =
      list.filter(_.isFile)

  private def readBytes(path: Path, options: ReadOptions): Source[ByteString] =
    Source.create[ByteString]{sink =>
      val fileChan = FileChannel.open(path, StandardOpenOption.READ)
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
          sink.next(ByteString.unsafeWrapOrCopy(buffer))
        }
        readBytes(channel, requestCount - 1, sink, options)
    else
      ()

  private[file] def onDirectoryStreamClose(path: Path): Unit = {}

  private[file] def readChannel(channel: ReadableByteChannel, buffer: ByteBuffer): Int =
    channel.read(buffer)

given PathOps with {}

extension (source: Source[Path])
  def zipWithAttributes[A](using getOp: GetAttributes[A]): Source[(Path, A)] =
    source.map(path => (path, getOp.attributesOf(path)))

given Conversion[Path, JNFPath] with {
  def apply(path: Path): JNFPath = {
    val root = if path.absolute then "/" else ""
    val head = root + path.elements.applyOrElse(0, _ => "")
    val tail = path.elements.drop(1)
    JNFPath.of(head, tail*)
  }
}

given Conversion[JNFPath, Path] with {
  def apply(path: JNFPath): Path = {
    Path(path.toString)
  }
}

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.OpenOption

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.given

import urwerk.concurrent.given

def read(path: JNFPath, options: OpenOption*)(using ec: ExecutionContext): Source[ByteString] =


  Source.create{sink =>
    val channel = AsynchronousFileChannel.open(path, options.toSet.asJava, ec.toExecutorService)
    val buffer = ByteBuffer.allocate(4096)
    channel.read(buffer, 0, (), ReadCompletionHandler())
  }

private class ReadCompletionHandler extends CompletionHandler[Integer, Any]:
  def completed(readCount: Integer, attachment: Any): Unit = ???
  def failed(error: Throwable, attachment: Any): Unit = ???

// private static class AsynchronousFileChannelReadCompletionHandler
//             implements CompletionHandler<integer, databuffer=""> {

//         private final AsynchronousFileChannel channel;

//         private final FluxSink<databuffer> sink;

//         private final DataBufferFactory dataBufferFactory;

//         private final int bufferSize;

//         private final AtomicLong position;

//         private final AtomicBoolean disposed = new AtomicBoolean();

//         public AsynchronousFileChannelReadCompletionHandler(AsynchronousFileChannel channel,
//                 FluxSink<databuffer> sink, long position, DataBufferFactory dataBufferFactory, int bufferSize) {

//             this.channel = channel;
//             this.sink = sink;
//             this.position = new AtomicLong(position);
//             this.dataBufferFactory = dataBufferFactory;
//             this.bufferSize = bufferSize;
//         }

//         @Override
//         public void completed(Integer read, DataBuffer dataBuffer) {
//             if (read != -1) {
//                 long pos = this.position.addAndGet(read);
//                 dataBuffer.writePosition(read);
//                 this.sink.next(dataBuffer);
//                 if (!this.disposed.get()) {
//                     DataBuffer newDataBuffer = this.dataBufferFactory.allocateBuffer(this.bufferSize);
//                     ByteBuffer newByteBuffer = newDataBuffer.asByteBuffer(0, this.bufferSize);
//                     this.channel.read(newByteBuffer, pos, newDataBuffer, this);
//                 }
//             }
//             else {
//                 release(dataBuffer);
//                 this.sink.complete();
//             }
//         }

//         @Override
//         public void failed(Throwable exc, DataBuffer dataBuffer) {
//             release(dataBuffer);
//             this.sink.error(exc);
//         }

//         public void dispose() {
//             this.disposed.set(true);
//         }
//     }

