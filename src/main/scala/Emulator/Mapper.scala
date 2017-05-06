package Emulator

import scala.annotation.switch
import scala.scalajs.js.Dynamic

/** The mapper class mainly handles address translation
  * It acts like like a cartridge board. There are
  * different types of mappers that vary from one game to another
  */
abstract class Mapper(nes: NES) {

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

  /** Write to memory or to specific register */
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
        if ((value & 1) == 0 && (joypadLastWrite & 1) == 1) {
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
    if ((addr & 0xffff) > 0x4017) {
      // ROM:
      nes.cpu.memory(addr)
    } else if ((addr & 0xffff) >= 0x2000) {
      // I/O Ports.
      regLoad(addr)
    } else {
      // RAM (mirrored)
      nes.cpu.memory(addr & 0x7FF)
    }
  }

  /** Load I/O register */
  def regLoad(addr: Int): Int = {
    val hnAddr: Int = (addr & 0xF000) >> 12 //Use the highest 4 bytes of address
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
            if (mousePressed) {
              // Check for white pixel nearby:
              val sx = Math.max(0, mouseX - 4)
              val ex = Math.min(256, mouseX + 4)
              val sy = Math.max(0, mouseY - 4)
              val ey = Math.min(240, mouseY + 4)
              var w = 0
              for (y <- sy to ey) {
                for (x <- sx to ex) {

                  if (nes.ppu.buffer((y << 8) + x) == 0xffffff) {
                    w |= 0x1 << 3
                    Dynamic.global.console("Clicked on white!")
                    return 0
                  }
                }
              }
              w |= (if (mousePressed) (0x1 << 4) else 0)
              (joy2Read | w) & 0xFFFF
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
    joy1StrobeState = (joy1StrobeState + 1) % 24
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
    joy2StrobeState = (joy2StrobeState + 1) % 24
    ret
  }

  /** Load ROM if valid */
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

  /** Load program rom into memory */
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

  /** Load graphical rom into memory */
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
      load4KVromBank(0, 0x0000)
      load4KVromBank(1, 0x1000)
      //}
    }
    else {
      Dynamic.global.console("No CHR-ROM banks found")
    }
  }

  /** Load battery ram if any. Unused for now */
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

  /** Load one program rom bank of 16KB */
  def loadRomBank(bank: Int, address: Int): Unit = {
    //default: 16KB banks
    // Loads a ROM bank into the specified address.
    //var data = this.nes.rom.rom[bank];
    //cpuMem.write(address,data,data.length);
    nes.rom.prgRom(bank % nes.rom.getPrgRomSize).copyToArray(nes.cpu.memory, address, 0x4000)
  }

  /** Load 2 program rom banks of a total of 32KB */
  def load32kRomBank(bank: Int, address: Int): Unit = {
    loadRomBank((bank * 2) % nes.rom.getPrgRomSize, address)
    loadRomBank((bank * 2 + 1) % nes.rom.getPrgRomSize, address + 0x4000)
  }

  /** Load 1/2 program rom bank of 8KB */
  def load8kRomBank(halfBank: Int, address: Int): Unit = {
    if (halfBank % 2 == 0) {
      nes.rom.prgRom((halfBank / 2) % nes.rom.getPrgRomSize).take(0x2000).copyToArray(nes.cpu.memory, address, 0x2000)
    } else {
      nes.rom.prgRom((halfBank / 2) % nes.rom.getPrgRomSize).drop(0x2000).copyToArray(nes.cpu.memory, address, 0x2000)
    }
  }

  /** Load 1 graphic rom bank of 8KB
    * the graphic bank is broken in 2 and loaded in 2 tiles
    * the first parameter is the number of the first half bank (4KB)
    */
  def loadVromBank(halfBank: Int, address: Int): Unit = {
    //default: 8KB banks
    if (nes.rom.getChrRomSize != 0) {
      //TODO or not: nes.ppu.triggerRendering
      load4KVromBank((halfBank) % (nes.rom.getChrRomSize * 2), address) //the size of our tiles are 4K (0x1000)
      load4KVromBank((halfBank + 1) % (nes.rom.getChrRomSize * 2), address + 0x1000)
    }
  }

  /** Load 1/2 graphic rom bank of 4KB and set a new Tile for it */
  def load4KVromBank(halfBank: Int, address: Int): Unit = {
    if (nes.rom.getChrRomSize != 0) {
      //TODO: nes.ppu.triggerRendering
      if (halfBank % 2 == 0) {
        nes.rom.chrRom((halfBank / 2) % nes.rom.getChrRomSize).take(0x1000).copyToArray(nes.ppu.vramMem, address, 0x1000)
      } else {
        nes.rom.chrRom((halfBank / 2) % nes.rom.getChrRomSize).drop(0x1000).copyToArray(nes.ppu.vramMem, address, 0x1000)
      }

      val vromTile: Array[Tile] = nes.rom.vromTile(halfBank % (nes.rom.getChrRomSize * 2))
      vromTile.copyToArray(nes.ppu.ptTile, address >> 4, 0x100)
    }
  }

  /** Load 1/4 graphic rom bank of 2KB and update corresponding tile */
  def load2kVromBank(quarterBank: Int, address: Int): Unit = {
    if (nes.rom.getChrRomSize != 0) {
      //TODO: nes.ppu.triggerRendering
      val offset = 0x800 * (quarterBank % 4)
      nes.rom.chrRom((quarterBank / 4) % nes.rom.getChrRomSize).slice(offset, offset + 0x800).copyToArray(nes.ppu.vramMem, address, 0x800)
      // Update tiles:
      val vromTile: Array[Tile] = nes.rom.vromTile((quarterBank / 2) % nes.rom.getChrRomSize) // Tiles are only half the size of a full graphic rom bank
      val baseIndex = address >> 4
      for (i <- 0 to 0x80) {
        nes.ppu.ptTile(baseIndex + i) = vromTile(((quarterBank % 2) << 7) + i)
      }
    }
  }

  /** Load 1/8 graphic rom bank of 1KB and update corresponding tile */
  def load1kVromBank(eighthBank: Int, address: Int): Unit = {
    if (nes.rom.getChrRomSize != 0) {
      //TODO: nes.ppu.triggerRendering
      val offset = 0x400 * (eighthBank % 8)
      nes.rom.chrRom((eighthBank / 8) % nes.rom.getChrRomSize).slice(offset, offset + 0x400).copyToArray(nes.ppu.vramMem, address, 0x400)
      // Update tiles:
      val vromTile: Array[Tile] = nes.rom.vromTile((eighthBank / 4) % nes.rom.getChrRomSize) // Tiles are only half the size of a full graphic rom bank
      val baseIndex = address >> 4
      for (i <- 0 to 0x40) {
        nes.ppu.ptTile(baseIndex + i) = vromTile(((eighthBank % 4) << 6) + i)
      }
    }
  }

  def clockIrqCounter: Unit = {
    // Does nothing. This is used by the MMC3 mapper.
  }

  def latchAccess(address: Int): Unit = {
    // Does nothing. This is used by MMC2.
  }


}

//Most common mapper: Nintendo MMC1
class MMC1(nes: NES) extends Mapper(nes) { // mapper number 1

  // 5-bit buffer:
  var regBuffer = 0
  var regBufferCounter = 0

  // Register 0:
  var mirroring = 0
  var oneScreenMirroring = 0
  var prgSwitchingArea = 1
  var prgSwitchingSize = 1
  var vromSwitchingSize = 0

  // Register 1:
  var romSelectionReg0 = 0

  // Register 2:
  var romSelectionReg1 = 0

  // Register 3:
  var romBankSelect = 0

  override def reset: Unit = {
    super.reset

    regBuffer = 0
    regBufferCounter = 0

    mirroring = 0
    oneScreenMirroring = 0
    prgSwitchingArea = 1
    prgSwitchingSize = 1
    vromSwitchingSize = 0

    romSelectionReg0 = 0

    romSelectionReg1 = 0

    romBankSelect = 0
  }

  override def write(address: Int, value: Byte): Unit = {
    // Writes to addresses other than MMC registers are handled by NoMapper.
    if (address < 0x8000) {
      super.write(address, value)
    } else {
      // See what should be done with the written value:
      if ((value & 128) != 0) {

        // Reset buffering:
        regBufferCounter = 0
        regBuffer = 0

        // Reset register:
        if (getRegNumber(address) === 0) {

          prgSwitchingArea = 1
          prgSwitchingSize = 1

        }
      }
      else {

        // Continue buffering:
        //regBuffer = (regBuffer & (0xFF-(1<<regBufferCounter))) | ((value & (1<<regBufferCounter))<<regBufferCounter)
        regBuffer = (regBuffer & (0xFF - (1 << regBufferCounter))) | ((value & 1) << regBufferCounter)
        regBufferCounter += 1

        if (regBufferCounter == 5) {
          // Use the buffered value:
          setReg(getRegNumber(address), regBuffer)

          // Reset buffer:
          regBuffer = 0
          regBufferCounter = 0
        }
      }
    }
  }

  def setReg(reg: Int,value: Int): Unit = {
    match (reg: @switch) {
      case 0 =>
      // Mirroring:
      val tmp: Int = value & 3
      if (tmp != mirroring) {
        // Set mirroring:
        mirroring = tmp
        if ((mirroring & 2) == 0) {
          // SingleScreen mirroring overrides the other setting:
          nes.ppu.setMirroring(nes.rom.SinglescreenMirroring)
        }
        // Not overridden by SingleScreen mirroring.
        else if ((mirroring & 1) != 0) {
          nes.ppu.setMirroring(nes.rom.HorizontalMirroring)
        }
        else {
          nes.ppu.setMirroring(nes.rom.VerticalMirroring)
        }
      }

      // PRG Switching Area
      prgSwitchingArea = (value >> 2) & 1

      // PRG Switching Size:
      prgSwitchingSize = (value >> 3) & 1

      // VROM Switching Size:
      vromSwitchingSize = (value >> 4) & 1

      case 1 =>
      // ROM selection:
      romSelectionReg0 = (value >> 4) & 1

      // Check whether the cart has VROM:
      if (nes.rom.getChrRomSize > 0) {

        // Select VROM bank at 0x0000:
        if (vromSwitchingSize == 0) {

          // Swap 8kB VROM:
          if (romSelectionReg0 == 0) {
            loadVromBank((value & 0xF), 0x0000)
          }
          else {
            loadVromBank(nes.rom.getChrRomSize + (value & 0xF),0x0000)
          }

        }
        else {
          // Swap 4kB VROM:
          if (romSelectionReg0 == 0) {
            load4KVromBank((value & 0xF), 0x0000)
          }
          else {
            load4KVromBank(nes.rom.getChrRomSize + (value & 0xF), 0x0000)
          }
        }
      }

      case 2 =>
      // ROM selection:
      romSelectionReg1 = (value >> 4) & 1

      // Check whether the cart has VROM:
      if (nes.rom.getChrRomSize > 0) {

        // Select VROM bank at 0x1000:
        if (vromSwitchingSize == 1) {
          // Swap 4kB of VROM:
          if (romSelectionReg1 == 0) {
            load4KVromBank((value & 0xF), 0x1000)
          }
          else {
            load4KVromBank(nes.rom.getChrRomSize + (value & 0xF), 0x1000)
          }
        }
      }
      case _ =>
        // Select ROM bank:
        // -------------------------
        val tmp: Int = value & 0xF
        var bank: Int = 0
        var baseBank: Int = 0

        if (nes.rom.getPrgRomSize >= 32) {
          // 1024 kB cart
          if (vromSwitchingSize == 0) {
            if (romSelectionReg0 == 1) {
              baseBank = 16
            }
          }
          else {
            baseBank = (romSelectionReg0 | (romSelectionReg1 << 1)) << 3
          }
        }
        else if (nes.rom.getPrgRomSize >= 16) {
          // 512 kB cart
          if (romSelectionReg0 == 1) {
            baseBank = 8
          }
        }

        if (prgSwitchingSize == 0) {
          // 32kB
          bank = baseBank + (value & 0xF)
          load32kRomBank(bank, 0x8000)
        }
        else {
          // 16kB
          bank = baseBank * 2 + (value & 0xF)
          if (prgSwitchingArea == 0) {
            loadRomBank(bank, 0xC000)
          }
          else {
            loadRomBank(bank, 0x8000)
          }
        }
    }
  }

  def getRegNumber(address: Int): Int = {
    if (address >= 0x8000 && address <= 0x9FFF) {
      0
    }
    else if (address >= 0xA000 && address <= 0xBFFF) {
      1
    }
    else if (address >= 0xC000 && address <= 0xDFFF) {
      2
    }
    else {
      3
    }
  }

  override def loadROM {
    if (!nes.rom.checkRom) {
      scalajs.js.Dynamic.global.alert("MMC1: Invalid ROM! Unable to load.")
    } else {

      // Load PRG-ROM:
      loadRomBank(0, 0x8000) //   First ROM bank..
      loadRomBank(nes.rom.getPrgRomSize - 1, 0xC000) // ..and last ROM bank.

      // Load CHR-ROM:
      loadCHRROM

      // Load Battery RAM (if present):
      loadBatteryRam

      // Do Reset-Interrupt:
      nes.cpu.requestIrq(2)
    }
  }
}

//Mapper for Castelvania 3, not yet fully working: Nintendo MMC5
class MMC5(nes: NES) extends Mapper(nes) { //mapper number 4
  var prg_size: Int = 0
  var chr_size: Int = 0
  var sram_we_a: Int = 0
  var sram_we_b: Int =  0
  var graphic_mode: Int = 0
  var nametable_mode: Int = 0
  var nametable_type: Array[Int] = _
  var fill_chr: Int = 0
  var fill_pal: Int = 0
  var chr_mode: Int = 0
  var chr_page: Array[Array[Int]] = _
  var split_control: Int = 0
  var split_scroll: Int = 0
  var split_page: Int = 0
  var irq_line: Int = 0
  var irq_enable: Int = 0
  var irq_status: Int = 0
  var mult_a: Int = 0
  var mult_b: Int = 0

  override def write(address: Int, value: Byte): Unit = {
    // Writes to addresses other than MMC registers are handled by NoMapper.
    if (address < 0x5000) {
      write(address, value)
    }
    else {
      (address: @switch) match {
        case 0x5100 => prg_size = value & 3
        case 0x5101 => chr_size = value & 3
        case 0x5102 => sram_we_a = value & 3
        case 0x5103 => sram_we_b = value & 3
        case 0x5104 => graphic_mode = value & 3
        case 0x5105 =>
          nametable_mode = value
          nametable_type(0) = value & 3
          //load1kVromBank(value & 3, 0x2000)
          var offset = value >> 2
          nametable_type(1) = offset & 3
          load1kVromBank(offset & 3, 0x2400)
          offset >>= 2
          nametable_type(2) = offset & 3
          load1kVromBank(offset & 3, 0x2800)
          offset >>= 2
          nametable_type(3) = offset & 3
          load1kVromBank(offset & 3, 0x2C00)
        case 0x5106 => fill_chr = value
        case 0x5107 => fill_pal = value & 3
        case 0x5113 => //SetBank_SRAM(3, value & 3)
        case 0x5114 | 0x5115 | 0x5116 | 0x5117 =>
        //SetBank_CPU(address, value)
        case 0x5120 | 0x5121 | 0x5122 | 0x5123 | 0x5124 | 0x5125 | 0x5126 | 0x5127=>
          chr_mode = 0
          chr_page(0)(address & 7) = value
        //SetBank_PPU ()
        case 0x5128 | 0x5129 | 0x512A | 0x512B =>
          chr_mode = 1
          chr_page(1)((address & 3) + 0) = value
          chr_page(1)((address & 3) + 4) = value
        //SetBank_PPU ()
        case 0x5200 => split_control = value
        case 0x5201 => split_scroll = value
        case 0x5202 => split_page = value & 0x3F
        case 0x5203 =>
          irq_line = value
          nes.cpu.clearIRQ
        case 0x5204 =>
          irq_enable = value
          nes.cpu.clearIRQ
        case 0x5205 => mult_a = value
        case 0x5206 => mult_b = value
        case _ =>
          if (address >= 0x5000 && address <= 0x5015) {
            nes.papu.exWrite(address, value)
          } else if (address >= 0x5C00 && address <= 0x5FFF) {
            if (graphic_mode == 2) {
              // ExRAM
              // vram write
            } else if (graphic_mode != 3) {
              // Split,ExGraphic
              if ((irq_status & 0x40) != 0) {
                // vram write
              } else {
                // vram write
              }
            }
          } else if (address >= 0x6000 && address <= 0x7FFF) {
            if (sram_we_a == 2 && sram_we_b == 1) {
              // additional ram write
            }
          }
      }
    }
  }

  override def loadROM: Unit = {
    if (!nes.rom.checkRom) {
      scalajs.js.Dynamic.global.alert("UNROM: Invalid ROM! Unable to load.")
    } else {

      // Load PRG-ROM:
      load8kRomBank(nes.rom.getPrgRomSize * 2 - 1, 0x8000)
      load8kRomBank(nes.rom.getPrgRomSize * 2 - 1, 0xA000)
      load8kRomBank(nes.rom.getPrgRomSize * 2 - 1, 0xC000)
      load8kRomBank(nes.rom.getPrgRomSize * 2 - 1, 0xE000)

      // Load CHR-ROM:
      loadCHRROM

      // Do Reset-Interrupt:
      nes.cpu.requestIrq(2)
    }
  }
}

