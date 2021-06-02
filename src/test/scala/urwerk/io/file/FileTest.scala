package urwerk.io.file

import org.reactivestreams.{Subscriber, Subscription}
import reactor.core.publisher.Flux
import reactor.core.scheduler.Schedulers
import urwerk.io.{ByteString, Path}
import urwerk.source.TestOps.{singletonProbe, sourceProbe}
import urwerk.source.{Singleton, Source, TestOps}
import urwerk.test.{TestBase, uniqueDirectory, uniqueFile, uniquePath}

import java.net.URI
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, NoSuchFileException, Path => JNFPath}

import scala.concurrent.ExecutionContext
import scala.io.Codec
import scala.jdk.CollectionConverters
import scala.util.Random
import scala.language.implicitConversions

class FileTest extends TestBase:

  "current working directory" in {
    Cwd.absolute should be (true)
    Path(sys.props("user.dir")) should be (Cwd)
  }

  Seq(
    (Path(""), JNFPath.of("")),
    (Path("/"), JNFPath.of("/")),
    (Path("/abc"), JNFPath.of("/abc")),
    (Path("abc/def"), JNFPath.of("abc/def")))
    .foreach{(givenPath, givenJnfPath) =>
      val suffix = s"from='$givenPath' to='$givenJnfPath'"

      s"conversion to java.nio.file.Path $suffix" in {
        val jnfPath: JNFPath = givenPath
        jnfPath should be (givenJnfPath)
        jnfPath.isAbsolute should be (givenPath.absolute)
      }

      s"conversion from java.nio.file.Path $suffix" in {
        val path: Path = givenJnfPath
        path should be (givenPath)
        path.absolute should be (givenJnfPath.isAbsolute)
      }
    }

  "root" in {
    Root.absolute should be (true)
    Root should be (Path("/"))
  }

  "attributes" in {
    singletonProbe(uniqueFile(Array[Byte](1, 2, 3))
        .attributes[BasicFileAttributes])
      .assertNext(attrs =>
        attrs.size should be (3)
        attrs.isRegularFile should be(true)
      )
  }

  "path by string" in {
    Path("abc", "def/ghi").elements.map(_.toString) should be(Seq("abc", "def", "ghi"))
  }

  "file bytes" in {
    val givenBytes = Random.nextBytes(10)
    val file = uniqueFile(givenBytes)

    file.bytes.reduce(_ ++ _).block.get should be(ByteString(givenBytes))
  }

  "file bytes with custom chunk size" in {
    given ReadOptions with {
      val chunkSize = 1
    }
    val givenBytes = Random.nextBytes(100)
    val file = uniqueFile(givenBytes)
    val givenBuffers = givenBytes.map(byte => ByteString(byte))

    file.bytes.toSeq.block should be (givenBuffers)
  }

  "file bytes fail with no such file" in {
    sourceProbe(
        uniquePath.bytes)
      .expectError(classOf[NoSuchFileException])
      .verify()
  }

  "file bytes fail with io exception on read" in {
    val file = uniqueFile(Array[Byte](1, 2, 3))
    given PathOps with {
      override private[file] def readChannel(channel: ReadableByteChannel, buffer: ByteBuffer): Int =
        throw IllegalStateException()
    }

    sourceProbe(
      file.bytes)
      .expectError(classOf[IllegalStateException])
      .verify()
  }

  "is file" in {
    uniqueFile.isFile should be (true)
    uniqueDirectory.isFile should be (false)
  }

  "is directory" in {
    uniqueFile.isDirectory should be (false)
    uniqueDirectory.isDirectory should be (true)
  }
  "list" - {
    val dir = uniqueDirectory
    val givenPaths = Set[Path](
      Files.createDirectory(dir / "dir-1"),
      Files.createFile(dir / "file-1"),
      Files.createDirectory(dir / "dir-2"),
      Files.createFile(dir / "file-2"))

    "pathes" in {
      var closed = false
      given PathOps with {
        override private[file] def onDirectoryStreamClose(path: Path): Unit =
          closed = true
      }

      val actualPaths = dir.list.toSeq.block
      actualPaths.toSet should be(givenPaths)
      closed should be(true)
    }
    "directories" in {
      val actualDirs = dir.directories.toSeq.block.toSet
      actualDirs should be(givenPaths.filter(Files.isDirectory(_)))
    }
    "files" in {
      val actualFiles = dir.files.toSeq.block.toSet
      actualFiles should be(givenPaths.filter(Files.isRegularFile(_)))
    }
  }

  "strings" in {
    given ReadOptions with {
      val chunkSize = 2
    }
    val file = uniqueFile("abc")
    sourceProbe(
        file.strings)
      .expectNext("ab", "c")
      .verifyComplete()
  }

  "zip with attributes" in {
    val givenSrc = Seq(uniqueFile, uniqueDirectory, uniqueFile, uniqueDirectory)
    val pathsWithAttrs = Source.from(givenSrc)
      .zipWithAttributes[BasicFileAttributes]
      .map((path: Path, attrs: BasicFileAttributes) => (path, attrs.isRegularFile, attrs.isDirectory))
      .toSeq.block

    val givenPathAttrs = givenSrc.map(path => (path, Files.isRegularFile(path), Files.isDirectory(path)))
    pathsWithAttrs should be(givenPathAttrs)
  }
