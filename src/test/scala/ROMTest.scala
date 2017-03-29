/**
  * Rom test class
  */

import Emulator.ROM
import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global


object ROMTest extends TestSuite {
  var rom = new ROM
  def tests = TestSuite {
    'OpenBadRomTests {
      var f: Future[Any] = rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
      val ret: Future[Any] = f.map {
        case _ =>
          assert(!rom.checkRom)
      }
      ret
    }
    'HeaderTests {
      val f: Future[Any] = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      val ret: Future[Any] = f.map {
        case _ =>
          assert(rom.checkRom)
          assert(rom.getMapperName == "Nintendo MMC5")
          assert(rom.getMirroringType == 0) //Even if I believed it was vertical, it looks like this rom is horizontal
          assert(!rom.hasTrainer)
          assert(rom.getChrRomSize == 16) //128k
          assert(rom.getPrgRomSize == 16) //256k
      }
      ret
    }
  }
  tests.runAsync().map {
    results =>
      assert(results.toSeq(0).value.isSuccess) // root
      assert(results.toSeq(1).value.isSuccess) // OpenBadRomTests
      assert(results.toSeq(2).value.isSuccess) // HeaderTests
  }
}