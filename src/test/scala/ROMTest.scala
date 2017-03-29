/**
  * Rom test class
  */

import Emulator.ROM
import org.scalajs.jquery.jQuery
import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object ROMTest extends TestSuite {
  var rom = new ROM
  def tests = TestSuite {
    'OpenRomTests {
      var f: Future[Any] = rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
      f.ready(Duration.Inf)
      assert(!rom.checkRom)

      /*
      f = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      f.onComplete {
        case Success(x) => assert(rom.checkRom)
        case Failure(e) => e.printStackTrace()
      }*/
    }
    'HeaderTests {
      val f: Future[Any] = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      f.ready(Duration.Inf)
      assert(rom.checkRom)
      /*f.onComplete {
        case Success(x) =>
          assert(rom.checkRom)
          assert(rom.getMapperName == "Nintendo MMC5")
          assert(rom.getMirroringType == 1)
          assert(!rom.hasTrainer)
          assert(rom.getChrRomSize == 16)
          assert(rom.getPrgRomSize == 16)
        case Failure(e) =>
          e.printStackTrace()
      }
      */
    }
  }
}