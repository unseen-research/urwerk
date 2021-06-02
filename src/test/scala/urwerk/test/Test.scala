package urwerk.test

import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

import urwerk.io.Path
import urwerk.io.file.given

import java.io.OutputStream
import java.nio.file.{Files, Paths}
import java.util.UUID

import scala.io.Codec
import scala.language.implicitConversions

@RunWith(classOf[JUnitRunner])
abstract class TestBase extends AnyFreeSpec with Matchers:

  reactor.util.Loggers.useJdkLoggers()

  def withOut(testFn: (OutputStream, OutputStream) => Unit): Unit = {
    val out = new java.io.ByteArrayOutputStream()
    val err = new java.io.ByteArrayOutputStream()
  
    Console.withOut(out){
      Console.withErr(err){
        testFn(out, err)
      }}
  }

def uniqueString: String = UUID.randomUUID().toString

def uniquePath: Path = Paths.get(s"build/tests/$uniqueString")

def uniqueDirectory: Path = Files.createDirectories(uniquePath)

def uniqueFile: Path =
  val path = uniquePath
  Files.createDirectories(path.getParent)
  Files.createFile(path)

def uniqueFile(bytes: Array[Byte]): Path =
  Files.write(uniqueFile, bytes)

def uniqueFile(content: String)(using codec: Codec): Path =
  Files.writeString(uniqueFile, content, codec.charSet)

