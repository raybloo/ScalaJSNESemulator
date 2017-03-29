/**
  * Rom test class
  */

import Emulator.ROM
import org.scalajs.jquery.jQuery

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ROMTest extends TestSuite {
  var rom = new ROM
  def tests = TestSuite {
    'OpenRomTests {
      var f: Future[Any] = rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
      Await.result(f,Duration.Inf)
      assert(!rom.checkRom)
      f = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      Await.result(f,Duration.Inf)
      assert(rom.checkRom)
    }
    'HeaderTest {
      val f: Future[Any] = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      Await.result(f,Duration.Inf)
      assert(rom.getMapperName == "Nintendo MMC5")
      assert(rom.getMirroringType == 1)
      assert(!rom.hasTrainer)
      assert(rom.getChrRomSize == 16)
      assert(rom.getPrgRomSize == 16)
    }
  }
}