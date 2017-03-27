/**
  * Rom test class
  */

import Emulator.ROM
import utest._

import org.scalajs.jquery.jQuery

object ROMTest extends TestSuite {
  var rom = new ROM
  def tests = TestSuite {
    'OpenRomTests {
      rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
      assert(!rom.checkRom)
    }
  }
}