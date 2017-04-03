package Emulator

import scala.annotation.switch
import scala.scalajs.js.Dynamic

/** The Cpu fetches, decodes and executes instructions
  * from de prgrom. It communicates to other parts of the
  * nes like the ppu and the rom
  */
class CPU(nes: NES) {

  //Memory
  var memory = new Array[Byte](0x10000)

  //Registers:
  var pc: Short = 0 //Program Counter (16 bits)
  var sp: Short = 0 //Stack Pointer (16 bits)
  var a: Byte = 0 //Accumulator (8 bits)
  var x: Byte = 0 //Index Register X (8 bits)
  var y: Byte = 0 //Index Register Y (8 bits)
  var p: Byte = 0 //Processor Status (8 bits)

  //Processor Status parts, separated for convenience
  var carryFlag: Boolean = false //0th bit
  var zeroFlag: Boolean = true //1st bit
  var interruptDisable: Boolean = true //2nd bit
  var decimalModeFlag: Boolean = false //3rd bit
  var breakCommand: Boolean = false //4th bit
  var unused: Boolean= false //5th bit
  var overflowFlag: Boolean = false //6th bit
  var negativeFlag: Boolean = false //7th bit

  //New Flags, some flags and the pc need to have a place to store their new values
  var pc_new: Short = 0
  var interruptDisable_new = false
  var unused_new: Boolean = false
  var breakCommand_new: Boolean = false

  //Additional emulator utility variables
  var opdata: Boolean = false
  var cyclesToHalt: Int = 0
  var crash: Boolean = false
  var irqRequested: Boolean = false
  var irqType: Int = 3
  /* irqType are:
    - 0 : normal
    - 1 : nmi
    - 2 : reset
    - anything else : undefined
   */

 //Maskable Interrupt
  var interrupt = null

  //Addressing Modes
  val ZERO_PAGE: Int = 0
  val RELATIVE: Int = 1
  val IMPLIED: Int = 2
  val ABSOLUTE: Int = 3
  val ACCUMULATOR: Int = 4
  val IMMEDIATE: Int = 5
  val INDEXED_ZERO_PAGE_X: Int = 6
  val INDEXED_ZERO_PAGE_Y: Int = 7
  val INDEXED_ABSOLUTE_X: Int = 8
  val INDEXED_ABSOLUTE_Y: Int = 9
  val PRE_INDEXED_INDIRECT: Int = 10
  val POST_INDEXED_INDIRECT: Int = 11
  val INDIRECT_ABSOLUTE: Int = 12



  /** Reset memory and all cpu flags and registers */
  def reset: Unit = {
    //memory reset
    memory = new Array[Byte](0x10000)

    //Internal RAM
    for(i <- 0 to 0x2000) {
      memory(i) =  -1 //0xFF
    }

    //Special Addresses
    for (i <- 0 to 4) {
      var i = p*0x800
      memory(i+0x008) = -9//0xF7
      memory(i+0x009) = -17//0xEF
      memory(i+0x00A) = -33//0xDF
      memory(i+0x00F) = -65//0xBF
    }

    //Everything else set to 0
    for(i <- 0x2001 to 0x8000) {
      memory(i) =  0x00
    }

    // CPU Registers:
    a = 0
    x = 0
    y = 0
    // Reset Stack pointer:
    //sp = -3 //0xFD
    sp = 0x01FF
    // Reset Program counter:
    pc = 0x7FFF
    pc_new = 0x7FFF
    // Reset Status register:
    p = 0x28
    setProcessorFlags(0x28)

    // Set flags:
    carryFlag = false
    decimalModeFlag = false
    interruptDisable = true
    interruptDisable_new = true
    overflowFlag = false
    negativeFlag = false
    zeroFlag = true
    unused = true
    unused_new = true
    breakCommand = true
    breakCommand_new = true
    p = getProcessorFlags()

    opdata = ??? //new opData
    cyclesToHalt = 0

    // Reset crash flag:
    crash = false

    // Interrupt notification:
    irqRequested = false
    irqType = 3

    var programRom = nes.rom.prgRom
  }

  /* I m not using that anymore, but I keep it somewhere just in case it was of some use
  /** Find where the program begins */
  def getResetVector(): Short = {
    loadMemory(-4,true) // 0xFFFC
  }
  */

  /** Get all the flags into one single Byte */
  def getProcessorFlags(): Byte = {
    var flags: Byte = 32
    if(carryFlag) {
      flags = (flags + 1).toByte
    }
    if(zeroFlag) {
      flags = (flags + 2).toByte
    }
    if(interruptDisable) {
      flags = (flags + 4).toByte
    }
    if(decimalModeFlag) {
      flags = (flags + 8).toByte
    }
    if(breakCommand) {
      flags = (flags + 16).toByte
    }
    if(unused) {
      flags = (flags + 32).toByte
    }
    if(overflowFlag) {
      flags = (flags + 64).toByte
    }
    if(negativeFlag) {
      flags = (flags + 128).toByte
    }
    flags
  }

  /** Set all processor flags according to one byte */
  def setProcessorFlags(pStatus: Byte): Unit = {
    carryFlag = (pStatus & 1) != 0
    zeroFlag = (pStatus & 2) != 0
    interruptDisable = (pStatus & 4) != 0
    decimalModeFlag = (pStatus & 8) != 0
    breakCommand = (pStatus & 16) != 0
    unused = (pStatus & 32) != 0
    overflowFlag = (pStatus & 64) != 0
    negativeFlag = (pStatus & 128) != 0
  }

  /**Load 1 byte from memory */
  def load1Word(address: Int): Int = {
    if (address < 0x2000) {
      memory(address & 0x7ff)
    }
    else {
      nes.mmap.load(address)
    }
  }

  /** Load 2 bytes from memory */
  def load2Words(address: Int): Int = {
    if (address < 0x1FFF) {
      (memory(address & 0x7FF) | (memory((address+1) & 0x7FF) << 8)).asInstanceOf[Short]
    }
    else {
      (nes.mmap.load(address) | (nes.mmap.load((address+1).asInstanceOf[Short]) << 8)).asInstanceOf[Short]
    }
  }

  /** Write in the memory */
  def write(address: Short, value: Byte): Unit = {
    if(address < 0x2000) {
      memory(address & 0x7FF) = value
    }
    else {
      nes.mmap.write(address,value)
    }
  }

  /** Emulate one instruction of the cpu */
  def emulate(): Unit = {
    var temp: Byte = 0
    var add: Byte = 0
    if(irqRequested) {
      temp = getProcessorFlags
      pc_new = pc
      interruptDisable_new = interruptDisable
      (irqType: @switch) match {
        case 0 => //normal interrupt
          if (!interruptDisable) {
            doIrq
          }
        case 1 => //non maskable interrupt
          doNmIrq
        case 2 => //reset
          doResetIrq
        case default =>
          Dynamic.global.console("Unspecified interrupt request type")
      }
      pc = pc_new
      interruptDisable = interruptDisable_new
      breakCommand = breakCommand_new
      irqRequested = false
    }
    var opinf: Int = nes.mmap.load((pc+1).toShort)
    var cycleCount: Int = (opinf >> 24)
    var cycleAdd: Int = 0

    //Find address mode
    var addrMode = (opinf >> 16) & 0xff

    //Increment PC by number of op bytes
    var opaddr = pc
    pc += (opinf >> 16) & 0xff
    var addr = 0
    (addrMode: @ switch) match {
      case ZERO_PAGE => //Use the address given after the opcode. zero page have no high byte
        addr = load1Word(opaddr+2)
      case RELATIVE => //Relative mode
        addr = load1Word(opaddr+2)
        if(addr<0x80) {
          addr += pc
        } else {
          addr += pc-0x0100
        }
      case ABSOLUTE => //Absolute mode, 2 bytes in the opcode used as index
        addr = load2Words(opaddr+2)
      case IMPLIED => //Address implied in instruction
      case ACCUMULATOR => //Address is in the accumulator of the processor
        addr = a
      case IMMEDIATE => //Immediate mode, the address is after the opcode
        addr = pc
      case INDEXED_ZERO_PAGE_X => //Zero Page Indexed mode X as index,
        addr = (load1Word(opaddr+2)+x)&0xff //like zero mode + register x
      case INDEXED_ZERO_PAGE_Y => //Zero Page Indexed mode Y as index,
        addr = (load1Word(opaddr+2)+y)&0xff //like zero mode + register x
      case INDEXED_ABSOLUTE_X => //Absolute Indexed mode X as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
      case INDEXED_ABSOLUTE_Y => //Absolute Indexed mode Y as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
      case PRE_INDEXED_INDIRECT => // Pre-indexed Indirect mode
        // Find the 16-bit address starting at the given location plus
        // the current X register. The value is the contents of that
        // address.
        addr = load1Word(opaddr+2)
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
        addr &= 0xff //load only from zero pages
        addr = load2Words(addr)
      case POST_INDEXED_INDIRECT => // Post-indexed Indirect mode
        // Find the 16-bit address contained in the given location
        // (and the one following). Add to that address the contents
        // of the Y register. Fetch the value
        // stored at that adress.
        addr = load2Words(load1Word(opaddr+2));
        if((addr&0xff00)!=((addr+y)&0xff00)){
          cycleAdd = 1
        }
        addr+=y
      case INDIRECT_ABSOLUTE => //Indirect Absolute mode, Find the 16-bit address contained at the given location
        addr = load2Words(opaddr+2)
        if(addr <= 0x1FFF) {
          addr = memory(addr) + (memory((addr & 0xff) | (((addr & 0xff) + 1) & 0xff)) << 8) //Read from address given in op
        } else {
          addr = nes.mmap.load(addr) + (nes.mmap.load((addr & 0xff) | (((addr & 0xff) + 1) & 0xff)) << 8) //When addr exceeds 0x1fff then it is mapped memory
        }
    }
  }

  /** Execute interrupt code */
  def doIrq: Unit = {

  }

  /** Execute unmaksable interrupt code */
  def doNmIrq: Unit = {

  }

  /** Execute reset interrupt code */
  def doResetIrq: Unit = {

  }

  def emulateCycle(): Int = {
    0
  }

}
