/**
  * Mapper test class
  */

import Emulator.{Mapper, NES, NoMapper, ROM}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MapperTest extends TestSuite {
  var nes: NES = new NES
  var mapper: Mapper = new NoMapper(nes)
  nes.mmap = mapper
  def tests = TestSuite {
    'LoadPrgRomTest {
      nes.rom.prgRom = Array.fill[Byte](3,0x4000)(42)
      nes.rom.prgRom(1) = Array.fill[Byte](0x4000)(52)
      nes.rom.prgRom(2) = Array.fill[Byte](0x4000)(113)
      nes.mmap.loadRomBank(2,0x8000)
      assert(nes.mmap.load(0xBFFF) == 113)
    }
  }

}