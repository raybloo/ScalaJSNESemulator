package Emulator

import scala.annotation.switch
import scala.scalajs.js.Dynamic

/** The mapper class mainly handles address translation
  * It acts like like a cartridge board. There are
  * different types of mappers that vary from one game to another
  */
class Mapper(mapper_type: Int,nes: NES) {

  //Devices State
  var joy1StrobeState = 0
  var joy2StrobeState = 0
  var joypadLastWrite = 0
  var mousePressed = false
  var mouseX = 0
  var mouseY = 0

  /** Reset state */
  def reset: Unit = {
    joy1StrobeState = 0
    joy2StrobeState = 0
    joypadLastWrite = 0
    mousePressed = false
    mouseX = 0
    mouseY = 0
  }

  /** Write to memory or to specific register*/
  def write(address: Int, value: Byte): Unit = {
    if (address < 0x2000) {
      // Mirroring of RAM:
      nes.cpu.memory(address & 0x7FF) = value
    }
    else if (address > 0x4017) {
      nes.cpu.memory(address) = value
      if (address >= 0x6000 && address < 0x8000) {
        // Write to SaveRAM. Store in file:
        //TODO:
        //if(nes.rom!=null) {
        //  nes.rom.writeBatteryRam(address,value)
        //}
      }
    }
    else if (address > 0x2007 && address < 0x4000) {
      regWrite(0x2000 + (address & 0x7), value)
    }
    else {
      regWrite(address, value)
    }
  }

  /** Same as write */
  def writeLow(address: Int, value: Byte) {
    if (address < 0x2000) {
      // Mirroring of RAM:
      nes.cpu.memory(address & 0x7FF) = value
    }
    else if (address > 0x4017) {
      nes.cpu.memory(address) = value
    }
    else if (address > 0x2007 && address < 0x4000) {
      regWrite(0x2000 + (address & 0x7), value)
    }
    else {
      regWrite(address, value)
    }
  }

  /** Write to I/O registers */
  def regWrite(address: Int, value: Byte) {
    (address: @switch) match {
      case 0x2000 =>
        // PPU Control register 1
        nes.cpu.memory(address) = value
        //nes.ppu.updateControlReg1(value)
      case 0x2001 =>
        // PPU Control register 2
        nes.cpu.memory(address) = value
        //nes.ppu.updateControlReg2(value)
      case 0x2003 =>
        // Set Sprite RAM address:
        //TODO: nes.ppu.writeSRAMAddress(value)
      case 0x2004 =>
        // Write to Sprite RAM:
        //TODO: nes.ppu.sramWrite(value)
      case 0x2005 =>
        // Screen Scroll offsets:
        //TODO: nes.ppu.scrollWrite(value)
      case 0x2006 =>
        // Set VRAM address:
        //TODO: nes.ppu.writeVRAMAddress(value)
      case 0x2007 =>
        // Write to VRAM:
        //TODO: nes.ppu.vramWrite(value)
      case 0x4014 =>
        // Sprite Memory DMA Access
      //TODO: nes.ppu.sramDMA(value)
      case 0x4015 =>
        // Sound Channel Switch, DMC Status
      //TODO: nes.papu.writeReg(address, value)
      case 0x4016 =>
        // Joystick 1 + Strobe
        if ((value&1) == 0 && (joypadLastWrite&1) == 1) {
          joy1StrobeState = 0
          joy2StrobeState = 0
        }
        joypadLastWrite = value
      case 0x4017 =>
        // Sound channel frame sequencer:
        //TODO: nes.papu.writeReg(address, value)
      case _ =>
      // Sound registers
      // console.log("write to sound reg")
      if (address >= 0x4000 && address <= 0x4017) {
        //TODO: nes.papu.writeReg(address,value)
      }
    }
  }

  /** Load ROM, mirrored RAM or I/O ports */
  def load(addr: Int): Int = {
    // Check address range:
    if ((addr&0xffff) > 0x4017) {
      // ROM:
      nes.cpu.memory(addr)
    } else if ((addr&0xffff) >= 0x2000) {
      // I/O Ports.
      regLoad(addr)
    } else {
      // RAM (mirrored)
      nes.cpu.memory(addr & 0x7FF)
    }
  }

  /** Load I/O register */
  def regLoad(addr: Int): Int = {
    val hnAddr: Int = (addr&0xF000) >> 12 //Use the highest 4 bytes of address
    (hnAddr: @switch) match {
      case 0 =>
        0
      case 1 =>
        0
      case 2 | 3 =>
        // PPU Registers
        ((addr & 0x07): @switch) match {
          case 0x0 =>
            // 0x2000:
            // PPU Control Register 1.
            // (the value is stored both
            // in main memory and in the
            // PPU as flags):
            // (not in the real NES)
            nes.cpu.memory(0x2000)
          case 0x1 =>
            // 0x2001:
            // PPU Control Register 2.
            // (the value is stored both
            // in main memory and in the
            // PPU as flags):
            // (not in the real NES)
            nes.cpu.memory(0x2001)
          case 0x2 =>
            // 0x2002:
            // PPU Status Register.
            // The value is stored in
            // main memory in addition
            // to as flags in the PPU.
            // (not in the real NES)
            //TODO: nes.ppu.readStatusRegister()
          case 0x3 =>
            0
          case 0x4 =>
            // 0x2004:
            // Sprite Memory read.
            //TODO: nes.ppu.sramLoad()
          case 0x5 =>
            0
          case 0x6 =>
            0
          case 0x7 =>
            // 0x2007:
            // VRAM read:
            //TODO: nes.ppu.vramLoad()
          case _ =>
            0 //Shouldn't be possible
        }
      case 4 =>
        // Sound+Joypad registers
        (((addr & 0xffff) - 0x4015): @switch) match {
          case 0 =>
            // 0x4015:
            // Sound channel enable, DMC Status
            //TODO: nes.papu.readReg(address)
          case 1 =>
            // 0x4016:
            // Joystick 1 + Strobe
            joy1Read
          case 2 =>
            // 0x4017:
            // Joystick 2 + Strobe
            if(mousePressed) {
              // Check for white pixel nearby:
              val sx = Math.max(0, mouseX - 4)
              val ex = Math.min(256, mouseX + 4)
              val sy = Math.max(0, mouseY - 4)
              val ey = Math.min(240, mouseY + 4)
              var w = 0
              for (y <- sy to ey) {
                for (x <- sx to ex) {

                  if (nes.ppu.buffer((y<<8)+x) == 0xffffff) {
                    w |= 0x1<<3
                    Dynamic.global.console("Clicked on white!")
                    return 0
                  }
                }
              }
              w |= (if(mousePressed) (0x1<<4) else 0)
              (joy2Read|w) & 0xFFFF
            }
            else {
              joy2Read
            }
          case _ =>
            0
        }
      case _ =>
        0
    }
    0
  }

  /** Read state of first joypad */
  def joy1Read: Int = {
    var ret: Int = 0
    (joy1StrobeState: @switch) match {
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 => // supported buttons
        ret = nes.keyboard.state1(joy1StrobeState)
      case 19 =>
        ret = 1
      case _ =>
        ret = 0
    }
    joy1StrobeState = (joy1StrobeState+1)%24
    ret
  }

  /** Read state of second joypad */
  def joy2Read: Int = {
    var ret: Int = 0
    (joy1StrobeState: @switch) match {
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 => // supported buttons
        ret = nes.keyboard.state2(joy2StrobeState)
      case 19 =>
        ret = 1
      case _ =>
        ret = 0
    }
    joy2StrobeState = (joy2StrobeState+1)%24
    ret
  }

  /** Load ROM if valid*/
  def loadROM: Unit = {
    if (!nes.rom.checkRom || nes.rom.getPrgRomSize < 1) {
      Dynamic.global.alert("NoMapper: Invalid ROM! Unable to load.")
    } else {
      loadPRGROM // Load ROM into memory
      loadCHRROM // Load CHR-ROM
      loadBatteryRam // Load Battery RAM (if present):
      nes.cpu.requestIrq(2) // Send a reset irq to the cpu
    }
  }

  def loadPRGROM: Unit = {
    if (nes.rom.getPrgRomSize > 1) {
      // Load the two first banks into memory.
      loadRomBank(0, 0x8000)
      loadRomBank(1, 0xC000)
    }
    else {
      // Load the one bank into both memory locations:
      loadRomBank(0, 0x8000)
      loadRomBank(0, 0xC000)
    }
  }

  def loadCHRROM: Unit = {
    if (nes.rom.getChrRomSize > 0) {
      //There is something weird with this if() condition
      //In the original emulator, the variable being compared
      //is called vromCount and counts the number of 4k banks
      //the rom has. Now this value could never be == 1 since
      //it was initialized as an int * 2...
      //I won't use it for now
      //if (nes.rom.getChrRomSize == 1) {
        //TODO: loadVromBank(0,0x0000)
        //TODO: loadVromBank(0,0x1000)
      //} else {
        //TODO: loadVromBank(0,0x0000)
        //TODO: loadVromBank(1,0x1000)
      //}
    }
    else {
      Dynamic.global.console("No CHR-ROM banks found")
    }
  }

  def loadBatteryRam: Unit = {
    if (nes.rom.hasBatteryRam) {
      /*
      var ram = nes.rom.batteryRam;
      if (ram !== null && ram.length == 0x2000) {
        // Load Battery RAM into memory:
        JSNES.Utils.copyArrayElements(ram, 0, nes.cpu.mem, 0x6000, 0x2000)
      }
      */
    } //I'll need more time to implement this, since I don't quite understand this
  }

  def loadRomBank(bank: Int, address: Int): Unit =  { //16KB banks
    // Loads a ROM bank into the specified address.
    //var data = this.nes.rom.rom[bank];
    //cpuMem.write(address,data,data.length);
    nes.rom.prgRom(bank % nes.rom.getPrgRomSize).copyToArray(nes.cpu.memory,address,0x4000)
  }

  def load4KVromBank(halfBank: Int, address: Int): Unit = {
    if (nes.rom.getChrRomSize != 0) {
      //TODO: nes.ppu.triggerRendering
      if(halfBank % 2 == 0) {
        nes.rom.chrRom((halfBank/2) % nes.rom.getChrRomSize).take(0x1000).copyToArray(nes.ppu.vramMem,address,0x1000)
      } else {
        nes.rom.chrRom((halfBank/2) % nes.rom.getChrRomSize).drop(0x1000).copyToArray(nes.ppu.vramMem,address,0x1000)
      }

      //TODO
      //var vromTile = nes.rom.vromTile(halfBank % nes.rom.getChrRomSize)
      //JSNES.Utils.copyArrayElements(vromTile, 0, this.nes.ppu.ptTile, address >> 4, 256);
    }
  }

}
