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
  nes.ppu.reset()
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
    'LoadChrRomTest {
      nes.rom.chrRom = Array.fill[Byte](2,0x2000)(0)
      nes.rom.chrRom(0) = Array.fill[Byte](0x2000)(52)
      nes.rom.chrRom(1) = Array.fill[Byte](0x2000)(128.toByte)
      nes.rom.chrRom(0)(0x1800) = 255.toByte
      nes.rom.chrRomSize = 2
      nes.rom.prgRomSize = 0
      nes.rom.getVromTiles
      nes.rom.fillInTiles
      nes.mmap.load4KVromBank(0,0x1000)
      nes.mmap.load4KVromBank(3,0x0000)
      nes.ppu.writeVRAMAddress(0x1f)
      nes.ppu.writeVRAMAddress(0xff) //read at 0x1fff
      nes.ppu.vramLoad() //store 52 into buffer
      nes.ppu.writeVRAMAddress(0x0f)
      nes.ppu.writeVRAMAddress(0xff)//read at 0x0fff
      assert(nes.ppu.vramLoad() == 52) //store 128 in the buffer
      nes.mmap.loadVromBank(0,0x0000)
      nes.ppu.writeVRAMAddress(0x18)
      nes.ppu.writeVRAMAddress(0x00) //read at 0x1800
      assert(nes.ppu.vramLoad() == 128) //store 255 in the buffer
      assert(nes.ppu.vramLoad() == 255)
    }
  }

}