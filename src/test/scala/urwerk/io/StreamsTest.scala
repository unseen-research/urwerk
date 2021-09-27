package urwerk.io

import java.io.ByteArrayInputStream

import scala.util.Random

import urwerk.test.TestBase
import urwerk.io.Streams.given

class StreamsTest extends TestBase:
  "input stream to source" in {
    val blockSize = Streams.DefaultBufferSize
    val givenBytes = Random.nextBytes(blockSize * 3)

    val givenByteStrings = Seq(
      ByteString.unsafeWrap(givenBytes, 0, blockSize),
      ByteString.unsafeWrap(givenBytes, blockSize, blockSize),
      ByteString.unsafeWrap(givenBytes, blockSize * 2, blockSize))

    val actualByteStrings = ByteArrayInputStream(givenBytes)
      .toSource.toSeq.block

    actualByteStrings should be (givenByteStrings)
  }

  "input stream to source with custom block size" in {
    val givenBytes = Random.nextBytes(100)
    val givenByteStrings = givenBytes.map(ByteString(_))

    val actualByteStrings = ByteArrayInputStream(givenBytes)
      .toSource(1).toSeq.block

    actualByteStrings should be (givenByteStrings)
  }