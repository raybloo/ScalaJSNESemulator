/**
  * Cpu test class
  */

import Emulator.{CPU, NES}
import utest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


object CPUTest extends TestSuite {
  var nes: NES = new NES
  var cpu = nes.cpu

  def tests = TestSuite {
    'LoadWriteTests {
      //I test only RAM address since above 0x2000 starts the mapped addresses and mapper is not yet fully implemented
      val addr1: Int = 0x0020
      val addr2: Int = 0x0031
      val addr3: Int = 0x0032
      cpu.write(addr1, 0xff.toByte)
      cpu.write(addr2, 0x10)
      cpu.write(addr3, 0x20)
      assert(cpu.load1Word(addr1) == 0xff)
      assert(cpu.load1Word(addr2) == 0x10)
      assert(cpu.load1Word(addr3) == 0x20)
      assert(cpu.load2Words(addr2) == 0x2010)
      cpu.reset
    }
    'ProcessorStatusTests {
      cpu.carryFlag = false
      cpu.zeroFlag = false
      cpu.negativeFlag = false
      cpu.unused = false
      cpu.overflowFlag = false
      cpu.breakCommand = false
      cpu.decimalModeFlag = false
      cpu.interruptDisable = false
      assert(cpu.getProcessorFlags == 0x00)
      cpu.setProcessorFlags(0xff.toByte)
      assert(cpu.getProcessorFlags == 0xff)
      cpu.reset
      assert(cpu.getProcessorFlags == 0x34) //0b00110100
    }
    'EmulateTest {
      val f: Future[Any] = nes.rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/tetr.nes")
      val ret: Future[Any] = f.map {
        case _ =>
          //ADC Absolute mode
          cpu.reset
          var addr = 0x0030
          cpu.pc = 20 //CMP, IMMEDIATE ADDRESSING MODE
          cpu.write(cpu.pc + 1, 0x6D.toByte) //op
          assert(cpu.load1Word(cpu.pc + 1) == 0x6D)
          cpu.write(cpu.pc + 2, addr.toByte) //addr
          cpu.write(cpu.pc + 3, 0x00) // cause mem is initialized to -1 for some reason
          assert(cpu.load2Words(cpu.pc + 2) == addr)
          cpu.write(addr, 2) //val
          cpu.a = 2
          cpu.emulate
          //println(s"Next Instruction is: ${cpu.OpData.instructions(nes.mmap.load(cpu.pc+1))}")
          assert(cpu.a == 4)

          //BCC Relative Mode
          cpu.reset
          cpu.pc = 0x0001
          cpu.write(cpu.pc + 1, 0x90.toByte) //op
          cpu.write(cpu.pc + 2, 0x10) //addr
          cpu.emulate
          assert(cpu.pc == 0x13)
      }
      ret
    }
  }
}