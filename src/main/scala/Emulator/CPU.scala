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
  //Bytes in scala are signed, so we'll have to unsign it when loading the memory

  //Registers:
  var pc: Int = 0 //Program Counter (16 bits)
  var sp: Int = 0 //Stack Pointer (16 bits)
  var a: Int = 0 //Accumulator (8 bits)
  var x: Int = 0 //Index Register X (8 bits)
  var y: Int = 0 //Index Register Y (8 bits)
  var p: Int = 0 //Processor Status (8 bits)

  //Processor Status parts, separated for convenience
  var carryFlag: Boolean = false //0th bit
  var zeroFlag: Boolean = false //1st bit
  var interruptDisable: Boolean = true //2nd bit
  var decimalModeFlag: Boolean = false //3rd bit
  var breakCommand: Boolean = false //4th bit
  var unused: Boolean= false //5th bit
  var overflowFlag: Boolean = false //6th bit
  var negativeFlag: Boolean = false //7th bit

  //New Flags, some flags and the pc need to have a place to store their new values
  var pc_new: Int = 0
  var interruptDisable_new: Boolean = false
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
  val ZERO_PAGE_XINDEXED: Int = 6
  val ZERO_PAGE_YINDEXED: Int = 7
  val ABSOLUTE_XINDEXED: Int = 8
  val ABSOLUTE_YINDEXED: Int = 9
  val XINDEXED_INDIRECT: Int = 10
  val INDIRECT_YINDEXED: Int = 11
  val INDIRECT: Int = 12



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
    zeroFlag = false
    unused = true
    unused_new = true
    breakCommand = true
    breakCommand_new = true
    p = getProcessorFlags

    opdata = ??? //new opData
    cyclesToHalt = 0

    // Reset crash flag:
    crash = false

    // Interrupt notification:
    irqRequested = false
    irqType = 3

    var programRom = nes.rom.prgRom
  }

  /** Get all the flags into one single Byte */
  def getProcessorFlags: Byte = {
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

  /** Return the unsigned value of a Byte in the form of an Int */
  def unsign(byte: Int): Int = {
    byte & 0xff
  }

  /**Load 1 byte from memory */
  def load1Word(address: Int): Int = {
    if (address < 0x2000) {
      unsign(memory(address & 0x7ff))
    }
    else {
      unsign(nes.mmap.load(address))
    }
  }

  /** Load 2 bytes from memory */
  def load2Words(address: Int): Int = {
    if (address < 0x1FFF) {
      unsign(memory(address & 0x7FF)) | (unsign(memory((address+1) & 0x7FF)) << 8)
    }
    else {
      unsign(nes.mmap.load(address)) | (unsign(nes.mmap.load(address+1)) << 8)
    }
  }

  /** Write in the memory */
  def write(address: Int, value: Byte): Unit = {
    if(address < 0x2000) {
      memory(address & 0x7FF) = value
    }
    else {
      nes.mmap.write(address,value)
    }
  }

  /** Emulate one instruction of the cpu */
  def emulate(): Unit = {
    var temp: Int = 0
    var add: Byte = 0
    if(irqRequested) {
      temp = getProcessorFlags
      pc_new = pc
      interruptDisable_new = interruptDisable
      (irqType: @switch) match {
        case 0 => //normal interrupt
          if (!interruptDisable) {
            doIrq(temp.toByte)
          }
        case 1 => //non maskable interrupt
          doNmIrq(temp.toByte)
        case 2 => //reset
          doResetIrq
        case _ =>
          Dynamic.global.console("Unspecified interrupt request type")
      }
      pc = pc_new
      interruptDisable = interruptDisable_new
      breakCommand = breakCommand_new
      irqRequested = false
    }
    val opinf: Int = nes.mmap.load((pc+1))
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
      case ZERO_PAGE_XINDEXED => //Zero Page Indexed mode X as index,
        addr = (load1Word(opaddr+2)+x)&0xff //like zero mode + register x
      case ZERO_PAGE_YINDEXED => //Zero Page Indexed mode Y as index,
        addr = (load1Word(opaddr+2)+y)&0xff //like zero mode + register x
      case ABSOLUTE_XINDEXED => //Absolute Indexed mode X as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
      case ABSOLUTE_YINDEXED => //Absolute Indexed mode Y as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
      case XINDEXED_INDIRECT => // Pre-indexed Indirect mode
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
      case INDIRECT_YINDEXED => // Post-indexed Indirect mode
        // Find the 16-bit address contained in the given location
        // (and the one following). Add to that address the contents
        // of the Y register. Fetch the value
        // stored at that adress.
        addr = load2Words(load1Word(opaddr+2));
        if((addr&0xff00)!=((addr+y)&0xff00)){
          cycleAdd = 1
        }
        addr+=y
      case INDIRECT => //Indirect Absolute mode, Find the 16-bit address contained at the given location
        addr = load2Words(opaddr+2)
        if(addr <= 0x1FFF) {
          addr = unsign(memory(addr)) + (unsign(memory((addr & 0xff)) | (((addr & 0xff) + 1) & 0xff)) << 8) //Read from address given in op
        } else {
          addr = unsign(nes.mmap.load(addr)) + (nes.mmap.load((addr & 0xff) | (((addr & 0xff) + 1) & 0xff)) << 8) //When addr exceeds 0x1fff then it is mapped memory
        }
      case _ =>
        Dynamic.global.console("Invalid Address Mode used")
    }
    addr&=0xffff //Address mustn't exceed 16 bits

    (opinf: @switch) match {
      case 0 => //ADC: Add with carry (memory and accumulator)
        temp = a + load1Word(addr) + (if(carryFlag) 1 else 0)
        overflowFlag = (!(((a ^ load1Word(addr)) & 0x80)!=0) && (((a ^ temp) & 0x80))!=0)
        carryFlag = temp > 0xff
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = a == 0
        a = temp & 0xff
        cycleCount += cycleAdd
      case 1 => //AND: And memory with accumulator, stores in accumulator
        a = a & load1Word(addr)
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = a == 0
        if(addrMode!=INDIRECT_YINDEXED) cycleCount += cycleAdd
      case 2 => //ASL: Shift left one bit
        if(addrMode == ACCUMULATOR) {
          carryFlag = (a & 0x80) != 0
          a = (a << 1) & 0xff
          negativeFlag = (a & 0x80) != 0
          zeroFlag = a == 0
        } else {
          temp = load1Word(addr)
          carryFlag = (temp & 0x80) != 0
          temp = (temp << 1) & 0xff
          negativeFlag = (temp & 0x80) != 0
          zeroFlag = temp == 0
          write(addr,temp.toByte)
        }
      case 3  => //BCC: Branch on carry clear
        if(!carryFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 4  => //BCS: Branch on carry set
        if(carryFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 5  => //BEQ: Branch on zero
        if(zeroFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 6  => //BIT: Test bits in memory with accumulator
        temp = load1Word(addr)
        negativeFlag = (temp & 0x80) != 0
        overflowFlag = (temp & 0x40) != 0
        temp &= a
        zeroFlag = temp == 0
      case 7  => //BMI: Branch on negative result
        if(negativeFlag) {
          cycleCount += 1
          pc = addr
        }
      case 8  => //BNE: Branch on not zero
        if(!zeroFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 9  => //BPL: Branch on positive result
        if(!negativeFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 10  => //BRK: Force Break
        pc += 2
        push((pc>>8).toByte);
        push(pc.toByte);
        breakCommand = true
        push(getProcessorFlags)
        interruptDisable = true
        pc = load2Words(0xfffe)
        pc -= 1
      case 11  => //BVC: Branch on overflow clear
        if(!overflowFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 12  => //BVS: Branch on overflow set
        if(overflowFlag) {
          cycleCount += (if((opaddr&0xff00)!=(addr&0xff00)) 2 else 1)
          pc = addr
        }
      case 13  => //CLC: Clear carry flag
        carryFlag = false
      case 14  => //CLD: Clear decimal flag
        decimalModeFlag = false
      case 15  => //CLI: Clear interrupt flag
        interruptDisable = false
      case 16  => //CLV: Clear overflow flag
        overflowFlag = false
      case 17  => //CMP: Compare memory and accumulator
        temp = a - load1Word(addr)
        carryFlag = temp >= 0
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
        cycleCount += cycleAdd
      case 18  => //CPX: Compare memory and index X
        temp = x - load1Word(addr)
        carryFlag = temp >= 0
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
      case 19  => //CPY: Compare memory and index Y
        temp = y - load1Word(addr)
        carryFlag = temp >= 0
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
      case 20  => //DEC: Decrement memory by one
        temp = (load1Word(addr)-1)
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
        write(addr,temp.toByte)
      case 21  => //DEX: Decrement index X by one
        x = (x-1)&0xff
        negativeFlag = (x & 0x80) != 0
        zeroFlag = x == 0
      case 22  => //DEY: Decrement index Y by one
        y = (y-1)&0xff
        negativeFlag = (y & 0x80) != 0
        zeroFlag = x == 0
      case 23  => //EOR: XOR Memory with accumulator, stores in accumulator
        a = load1Word(addr)^a
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
        cycleCount += cycleAdd
      case 24  => //INC: Increment memory by one
        temp = (load1Word(addr)+1)
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
        write(addr,temp.toByte)
      case 25  => //INX: Increment index X by one
        x = (x+1)&0xff
        negativeFlag = (x & 0x80) != 0
        zeroFlag = x == 0
      case 26  => //INY: Increment index Y by one
        y = (y+1)&0xff
        negativeFlag = (y & 0x80) != 0
        zeroFlag = y == 0
      case 27  => //JMP: Jump to new location
        pc = addr-1
      case 28  => //JSR: Jump to new location, pushing the 2 address bytes on the stack
        push((pc>>8).toByte)
        push(pc.toByte)
        pc = addr-1
      case 29 => //LDA: Load accumulator with memory
        a = load1Word(addr)
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
      case 30 => //LDX: Load index X with memory
        x = load1Word(addr)
        negativeFlag = (x & 0x80) != 0
        zeroFlag = x == 0
      case 31 => //LDY: Load index Y with memory
        y = load1Word(addr)
        negativeFlag = (y & 0x80) != 0
        zeroFlag = y == 0
      case 32 => //LSR: Shift right one bit
        if(addrMode == ACCUMULATOR) {
          carryFlag = (a & 0x01) != 0
          a = (a >> 1) & 0xff
          zeroFlag = a == 0
        } else {
          temp = load1Word(addr)
          carryFlag = (temp & 0x01) != 0
          temp = (temp >> 1) & 0xff
          zeroFlag = temp == 0
          write(addr,temp.toByte)
        }
        negativeFlag = false //when right shifting, the sign bit always becomes 0
      case 33 => //NOP: No operation
      case 34 => //ORA: OR memory with accumulator, stores in accumulator
        temp = (load1Word(addr)|a)&0xff
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = temp == 0
        if(addrMode!=INDIRECT_YINDEXED) cycleCount += cycleAdd
      case 35 => //PHA: Push accumulator on stack
        push(a.toByte)
      case 36 => //PHP: Push processor status on stack
        breakCommand = true
        push(getProcessorFlags)
      case 37 => //PLA: Pop accumulator from stack
        a = pop
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
      case 38 => //PLP: Pop processor status from stack
        setProcessorFlags(pop.toByte)
      case 39 => //ROL: Rotate one bit left
        if(addrMode == ACCUMULATOR){
          temp = a
          add = if(carryFlag) 1 else 0
          carryFlag = (temp & 0x80) != 0
          temp = ((temp<<1)&0xff)+add
          a = temp
        } else {
          temp = load1Word(addr)
          add = if(carryFlag) 1 else 0
          carryFlag = (temp & 0x80) != 0
          temp = ((temp<<1)&0xff)+add
          write(addr, temp.toByte)
        }
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = temp == 0
      case 40 => //ROR: Rotate one bit right
        if(addrMode == ACCUMULATOR){
          add = if(carryFlag) 1 else 0
          carryFlag = (a & 0x01) != 0
          temp = ((a>>1)&0xff)+(add<<7)
          a = temp
        } else {
          temp = load1Word(addr)
          add = if(carryFlag) 1 else 0
          carryFlag = (temp & 0x01) != 0
          temp = ((temp>>1)&0xff)+(add<<7)
          write(addr, temp.toByte)
        }
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = temp == 0
      case 41 => //RTI: Return from interrupt, pop status and pc from stack
        setProcessorFlags(pop.toByte)
        pc = pop
        pc += (pop<<8)
        if(pc == 0xffff) {
          //TODO exit function
          return
        }
        pc -= 1
        unused = true
      case 42 => //RTS: Return from subroutine, pop pc from stack
        pc = pop
        pc += (pop<<8)
        if(pc == 0xffff) {//return from NSF play routine
          //TODO exit function
          return
        }
      case 43 => //SBC: Subtract memory from accumulator with borrow
      case _ =>
        Dynamic.global.console("Invalid op")

    }
  }



  /** Execute interrupt code */
  def doIrq(status: Byte): Unit = {

  }

  /** Execute unmaksable interrupt code */
  def doNmIrq(status: Byte): Unit = {

  }

  /** Execute reset interrupt code */
  def doResetIrq: Unit = {

  }

  /** Push a value on the stack */
  def push(value: Byte): Unit = {
    nes.mmap.write(sp,value)
    sp -= 1
    stackwrap
  }

  /** Pop the top value of the stack */
  def pop: Int = {
    sp += 1
    stackwrap
    nes.mmap.load(sp)
  }

  /** Wrap the stack pointer */
  def stackwrap = {
    sp = 0x0100 | (sp&0xff)
  }

  def emulateCycle(): Int = {
    0
  }

}
