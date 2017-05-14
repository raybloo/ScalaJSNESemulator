/**
  * Mapper test class
  */

import Emulator.{Mapper, NES, NoMapper, ROM}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import utest._

object MapperTest extends TestSuite {
  var nes: NES = new NES
  var mapper: Mapper = new NoMapper(nes)
  nes.mmap = mapper
  def tests = TestSuite {
    'LoadPrgRomTest {
      nes.rom.prgRom = Array.fill[Byte](3,0x4000)(42)
      nes.rom.prgRom(1) = Array.fill[Byte](0x4000)(52)
      nes.rom.prgRom(2) = Array.fill[Byte](0x4000)(128.toByte)
      nes.rom.chrRomSize = 0
      nes.rom.prgRomSize = 3
      nes.mmap.loadRomBank(2,0x8000)
      nes.mmap.loadRomBank(0,0xC000)
      assert(nes.mmap.load(0xBFFF) == 128)
      assert(nes.cpu.load2Words(0xFFFC) == 42 + (42 << 8))
    }
  }

}