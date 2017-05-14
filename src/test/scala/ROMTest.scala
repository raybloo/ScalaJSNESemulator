/**
  * Rom test class
  */

import Emulator.{NES, NoMapper, ROM}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import utest._


object ROMTest extends TestSuite {
  var nes: NES = new NES
  var rom = new ROM(nes)
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
          assert(rom.getMirroringType == rom.HorizontalMirroring) //Even if I believed it was vertical, it looks like this rom is horizontal
          assert(!rom.hasTrainer)
          assert(!rom.hasBatteryRam)
          assert(rom.getChrRomSize == 16) //128k
          assert(rom.getPrgRomSize == 16) //256k
      }
      ret
    }
    'GetRomTests {
      val f: Future[Any] = rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/tetr.nes")
      val ret: Future[Any] = f.map {
        case _ =>
          assert(rom.checkRom)
          assert(rom.getPrgRomSize == 2) //32k
          val prgROM: Array[Array[Byte]] = rom.getPrgRom
          assert(prgROM(0)(0x3fff) == rom.prgRom(0)(0x3fff))
          assert(prgROM(1)(0x3fff) == rom.prgRom(1)(0x3fff))
          assert(prgROM(1)(2) != 0)
          assert(prgROM.length == 2)
          assert(prgROM(0).length == 0x4000)
          val chrROM: Array[Array[Byte]] = rom.getChrRom
          assert(chrROM(0)(0x1fff) == rom.chrRom(0)(0x1fff))
          assert(chrROM(1)(0x1fff) == rom.chrRom(1)(0x1fff))
          assert(chrROM.length == 2)
          assert(chrROM(0).length == 0x2000)

      }
      ret
    }
  }

}