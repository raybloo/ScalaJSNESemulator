/**
  * Rom test class
  */

import Emulator.ROM
import utest._

import org.scalajs.jquery.jQuery

object ROMTest extends TestSuite {
  // Initialize App
  def tests = TestSuite {
    var rom = new ROM
    'OpenRomTests {
      rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
      assert(!rom.checkRom)
      rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes")
      assert(rom.checkRom)
    }
  }
}