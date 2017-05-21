package Emulator

import scala.annotation.switch
import scala.scalajs.js.Dynamic

/** The Cpu fetches, decodes and executes instructions
  * from de prgrom. It communicates to other parts of the
  * nes like the ppu and the rom mainly via the mapper
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
  var breakCommand: Boolean = true //4th bit
  var unused: Boolean = true //5th bit
  var overflowFlag: Boolean = false //6th bit
  var negativeFlag: Boolean = false //7th bit

  //New Flags, some flags and the pc need to have a place to store their new values (useful, when returning from interrupts)
  var pc_new: Int = 0
  var interruptDisable_new: Boolean = true
  var unused_new: Boolean = true
  var breakCommand_new: Boolean = true

  //Additional emulator utility variables
  OpData.init
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

  /** Reset memory and all cpu flags and registers */
  def reset: Unit = {
    //memory reset
    memory = new Array[Byte](0x10000)

    //Internal RAM
    for(i <- 0 until 0x2000) {
      memory(i) =  -1 //0xFF
    }

    //Special Addresses
    for (p <- 0 until 4) {
      val i = p*0x800
      memory(i+0x008) = -9//0xF7
      memory(i+0x009) = -17//0xEF
      memory(i+0x00A) = -33//0xDF
      memory(i+0x00F) = -65//0xBF
    }

    //Everything else set to 0
    for(i <- 0x2001 until memory.length) {
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

    // Set flags:
    carryFlag = false
    zeroFlag = false
    interruptDisable = true
    decimalModeFlag = false
    breakCommand = true
    unused = true
    overflowFlag = false
    negativeFlag = false

    // Set flag buffers:
    interruptDisable_new = true
    unused_new = true
    breakCommand_new = true
    p = getProcessorFlags

    OpData.init
    cyclesToHalt = 0

    // Reset crash flag:
    crash = false

    // Interrupt notification:
    irqRequested = false
    irqType = 3

  }

  /** Get all the flags into one single Byte */
  def getProcessorFlags: Int = {
    var flags: Int = 0
    if(carryFlag) {
      flags = flags + 1
    }
    if(zeroFlag) {
      flags = flags + 2
    }
    if(interruptDisable) {
      flags = flags + 4
    }
    if(decimalModeFlag) {
      flags = flags + 8
    }
    if(breakCommand) {
      flags = flags + 16
    }
    if(unused) {
      flags = flags + 32
    }
    if(overflowFlag) {
      flags = flags + 64
    }
    if(negativeFlag) {
      flags = flags + 128
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
    (byte & 0xff)
  }

  /**Load 1 byte from memory */
  def load1Word(address: Int): Int = {
    if (address < 0x2000) {
      unsign(memory(address & 0x7ff)) //address under 0x2000 are mirrored every 0x800
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

  /** Emulate one instruction of the cpu
    *  return number of cycles
    */
  def emulate: Int = {
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
          Dynamic.global.console.log("Unspecified interrupt request type")
      }
      pc = pc_new
      interruptDisable = interruptDisable_new
      breakCommand = breakCommand_new
      irqRequested = false
    }

    //Get next instruction of the program
    val op: Int = unsign(nes.mmap.load(pc+1))

    //Retrieve op infos from opdata object
    val opinf: Int = OpData.instructions(op)

    //Save the cycle cost of the instruction (may still change at this point)
    var cycleCount: Int = OpData.cycles(op)
    var cycleAdd: Int = 0

    //Find address mode
    val addrMode = (OpData.addressingMode(op) & 0xff)

    //Increment PC by number of op bytes
    var opaddr = pc
    pc += (OpData.opSize(op) & 0xff)


    //Find the effective address
    var addr: Int = 0
    (addrMode: @switch) match {
      case OpData.ZERO_PAGE => //Use the address given after the opcode. zero page have no high byte
        addr = load1Word(opaddr+2)
      case OpData.RELATIVE => //Relative mode, address is relative to pc
        addr = load1Word(opaddr+2)
        if(addr<0x80) {
          addr += pc
        } else {
          addr += pc-0x0100
        }
      case OpData.ABSOLUTE => //Absolute mode, 2 bytes in the opcode used as index
        addr = load2Words(opaddr+2)
      case OpData.IMPLIED => //Address implied in instruction
      case OpData.ACCUMULATOR => //Address is in the accumulator of the processor
        addr = a
      case OpData.IMMEDIATE => //Immediate mode, the address is after the opcode
        addr = pc
      case OpData.ZERO_PAGE_XINDEXED => //Zero Page Indexed mode X as index,
        addr = (load1Word(opaddr+2)+x)&0xff //like zero mode + register x
      case OpData.ZERO_PAGE_YINDEXED => //Zero Page Indexed mode Y as index,
        addr = (load1Word(opaddr+2)+y)&0xff //like zero mode + register x
      case OpData.ABSOLUTE_XINDEXED => //Absolute Indexed mode X as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+x)&0xff00)){
          cycleAdd = 1
        }
        addr+=x
      case OpData.ABSOLUTE_YINDEXED => //Absolute Indexed mode Y as index
        addr = load2Words(opaddr+2) //like Zero Page Indexed but with the high byte
        if((addr&0xff00)!=((addr+y)&0xff00)){
          cycleAdd = 1
        }
        addr+=y
      case OpData.XINDEXED_INDIRECT => // Pre-indexed Indirect mode
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
      case OpData.INDIRECT_YINDEXED => // Post-indexed Indirect mode
        // Find the 16-bit address contained in the given location
        // (and the one following). Add to that address the contents
        // of the Y register. Fetch the value
        // stored at that adress.
        addr = load2Words(load1Word(opaddr+2))
        if((addr&0xff00)!=((addr+y)&0xff00)){
          cycleAdd = 1
        }
        addr+=y
      case OpData.INDIRECT => //Indirect Absolute mode, Find the 16-bit address contained at the given location
        addr = load2Words(opaddr+2)
        if(addr < 0x1FFF) {
          addr = unsign(memory(addr)) + (unsign(memory((addr & 0xff00) | (((addr & 0xff) + 1) & 0xff))) << 8) //Read from address given in op
        } else {
          addr = unsign(nes.mmap.load(addr)) + (nes.mmap.load((addr & 0xff00) | (((addr & 0xff) + 1) & 0xff)) << 8) //When addr exceeds 0x1fff then it is mapped memory
        }
      case _ =>
        nes.stop
        Dynamic.global.console.log(s"ERROR: Invalid Address Mode $addrMode")
        Dynamic.global.console.log(s"caused by Op code $op")
        Dynamic.global.console.log(s"at Op address $opaddr")
    }
    addr&=0xffff //Address mustn't exceed 16 bits

    //Instruction code
    (opinf: @switch) match {
      case 0 => //ADC: Add with carry (memory and accumulator)
        temp = a + load1Word(addr) + (if(carryFlag) 1 else 0)
        overflowFlag = (!(((a ^ load1Word(addr)) & 0x80)!=0) && (((a ^ temp) & 0x80))!=0)
        carryFlag = temp > 0xff
        negativeFlag = (temp & 0x80) != 0
        a = temp & 0xff
        zeroFlag = a == 0
        cycleCount += cycleAdd
      case 1 => //AND: And memory with accumulator, stores in accumulator
        a = a & load1Word(addr)
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = a == 0
        if(addrMode!=OpData.INDIRECT_YINDEXED) cycleCount += cycleAdd
      case 2 => //ASL: Shift left one bit
        if(addrMode == OpData.ACCUMULATOR) {
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
        push((pc>>8).toByte)
        push(pc.toByte)
        breakCommand = true
        push(getProcessorFlags.toByte)
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
        zeroFlag = temp == 0
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
      case 28  => //JSR: Jump to new location, pushing the return address on the stack
        push((pc>>8).toByte)
        push(pc.toByte)
        pc = addr-1
      case 29 => //LDA: Load accumulator with memory
        a = load1Word(addr)
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
        cycleCount += cycleAdd
      case 30 => //LDX: Load index X with memory
        x = load1Word(addr)
        negativeFlag = (x & 0x80) != 0
        zeroFlag = x == 0
        cycleCount += cycleAdd
      case 31 => //LDY: Load index Y with memory
        y = load1Word(addr)
        negativeFlag = (y & 0x80) != 0
        zeroFlag = y == 0
        cycleCount += cycleAdd
      case 32 => //LSR: Shift right one bit
        if(addrMode == OpData.ACCUMULATOR) {
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
        a = (load1Word(addr)|a)&0xff
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
        if(addrMode!=OpData.INDIRECT_YINDEXED) cycleCount += cycleAdd
      case 35 => //PHA: Push accumulator on stack
        push(a.toByte)
      case 36 => //PHP: Push processor status on stack
        breakCommand = true
        push(getProcessorFlags.toByte)
      case 37 => //PLA: Pop accumulator from stack
        a = pop
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
      case 38 => //PLP: Pop processor status from stack
        setProcessorFlags(pop.toByte)
        unused = true
      case 39 => //ROL: Rotate one bit left
        if(addrMode == OpData.ACCUMULATOR){
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
        if(addrMode == OpData.ACCUMULATOR){
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
          return 0
        }
        pc -= 1
        unused = true
      case 42 => //RTS: Return from subroutine, pop pc from stack
        pc = pop
        pc += (pop<<8)
        if(pc == 0xffff) {//return from NSF play routine
          //TODO exit function
          return 0
        }
      case 43 => //SBC: Subtract memory from accumulator with borrow
        temp = a-load1Word(addr)-(if(carryFlag)0 else 1)
        negativeFlag = (temp & 0x80) != 0
        zeroFlag = (temp&0xff) == 0
        overflowFlag = ((((a^temp)&0x80)!=0 && ((a^load1Word(addr))&0x80)!=0))
        carryFlag = temp < 0
        a = temp&0xff
        if(addrMode!=OpData.INDIRECT_YINDEXED) cycleCount += cycleAdd
      case 44 => //SEC: Set carry flag
        carryFlag = true
      case 45 => //SED: Set decimal flag
        decimalModeFlag = true
      case 46 => //SEI: Set interrupt disable status
        interruptDisable = true
      case 47 => //STA: Store accumulator in memory
        write(addr,a.toByte)
      case 48 => //STX: Store index X in memory
        write(addr,x.toByte)
      case 49 => //STY: Store index Y in memory
        write(addr,y.toByte)
      case 50 => //TAX: Transfer accumulator to index X
        x = a
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
      case 51 => //TAY: Transfer accumulator to index Y
        y = a
        negativeFlag = (a & 0x80) != 0
        zeroFlag = a == 0
      case 52 => //TSX: Transfer stack pointer to index X
        x = (sp-0x0100)
        negativeFlag = (sp & 0x80) != 0
        zeroFlag = x == 0
      case 53 => //TXA: Transfer index X to accumulator
        a = x
        negativeFlag = (x & 0x80) != 0
        zeroFlag = x == 0
      case 54 => //TXS: Transfer index X to stack pointer
        sp = (x+0x0100)
        stackwrap
      case 55 => //TYA: Transfer index y to accumulator
        a = y
        negativeFlag = (y & 0x80) != 0
        zeroFlag = y == 0
      case _ =>
        nes.stop
        Dynamic.global.console.log(s"ERROR: Invalid Operation $op")
        Dynamic.global.console.log(s"at $opaddr")
    }
    //Dynamic.global.console.log(s"Op #$opinf has been executed")
    //Dynamic.global.console.log(s"with addressing mode #$addrMode")
    cycleCount
  }

  /** Request interrupt */
  def requestIrq(irType: Int): Unit = {
    if(!(irqRequested && irType == 0)) { //normal interrupt type
      irqRequested = true
      irqType = irType
    }
  }

  /** Indicates whether 2 addresses points to the same 1/4 KB of memory */
  def pageCrossed(addr1: Int, addr2: Int): Boolean = {
    (addr1&0xff00)!=(addr2&0xff00)
  }

  /** Execute interrupt code */
  def doIrq(status: Byte): Unit = {
    pc_new += 1
    push((pc_new>>8).toByte)
    push(pc_new.toByte)
    push(status)
    interruptDisable_new = true
    breakCommand_new = false
    pc_new = nes.mmap.load(0xfffe) | (nes.mmap.load(0xffff) << 8)
    pc_new -= 1
    //Dynamic.global.console.log("NORMAL IRQ")
  }

  /** Execute unmaksable interrupt code */
  def doNmIrq(status: Byte): Unit = {
    if ((nes.mmap.load(0x2000) & 0x80) != 0) { // Check whether VBlank Interrupts are enabled
      pc_new += 1
      push((pc_new >> 8).toByte)
      push(pc_new.toByte)
      push(status)
      pc_new = nes.mmap.load(0xfffa) | (nes.mmap.load(0xfffb) << 8)
      pc_new -= 1
      //Dynamic.global.console.log("NM IRQ")
    }
  }

  /** Execute reset interrupt code */
  def doResetIrq: Unit = {
    pc_new = nes.mmap.load(0xfffc) | (nes.mmap.load(0xfffd) << 8)
    Dynamic.global.console.log(s"RESET VECTOR IS: ${nes.mmap.load(0xfffc) | (nes.mmap.load(0xfffd) << 8)}")
    pc_new -= 1
    //Dynamic.global.console.log("RESET IRQ")
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
  def stackwrap: Unit = {
    sp = 0x0100 | (sp&0xff)
  }

  /* Increment the number of cycle to halt */
  def haltCycles(cycles: Int): Unit = {
    cyclesToHalt += cycles
  }

  /** Clears pending irq */
  def clearIRQ: Unit = {
    //not yet implemented
  }

  /** Contains all the data about the opcode in 4 arrays
    * every instruction has its own:
    *   - number of instruction (for our switch)
    *   - set of valid addressingMode
    *   - size (parameters)
    *   - number of cycles it takes to process
    * This data was originally stored in only one array with all 4 infos
    * stored on the same Int, but I split it for convenience and readability
    */
  object OpData {
    //Default: opdata is filled with invalid values before being initialized
    var instructions: Array[Int] = Array.fill[Int](0x0100)(-1)
    var addressingMode: Array[Int] = Array.fill[Int](0x0100)(-1)
    var opSize: Array[Int] = Array.fill[Int](0x0100)(-1)
    var cycles: Array[Int] = Array.fill[Int](0x0100)(-1)

    //Addressing Modes
    final val ZERO_PAGE = 0
    final val RELATIVE = 1
    final val IMPLIED = 2
    final val ABSOLUTE = 3
    final val ACCUMULATOR = 4
    final val IMMEDIATE = 5
    final val ZERO_PAGE_XINDEXED = 6
    final val ZERO_PAGE_YINDEXED = 7
    final val ABSOLUTE_XINDEXED = 8
    final val ABSOLUTE_YINDEXED = 9
    final val XINDEXED_INDIRECT = 10
    final val INDIRECT_YINDEXED = 11
    final val INDIRECT = 12

    def setOp(opAddr: Int,instr: Int,addrMode: Int,size: Int,cycle: Int): Unit = {
      instructions(opAddr) = instr
      addressingMode(opAddr) = addrMode
      opSize(opAddr) = size
      cycles(opAddr) = cycle
    }

    def init: Unit = {
      //Fill in all valid opcodes:

      // ADC:
      setOp(0x69,0,IMMEDIATE,2,2)
      setOp(0x65,0,ZERO_PAGE,2,3)
      setOp(0x75,0,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x6D,0,ABSOLUTE,3,4)
      setOp(0x7D,0,ABSOLUTE_XINDEXED,3,4)
      setOp(0x79,0,ABSOLUTE_YINDEXED,3,4)
      setOp(0x61,0,XINDEXED_INDIRECT,2,6)
      setOp(0x71,0,INDIRECT_YINDEXED,2,5)

      // AND:
      setOp(0x29,1,IMMEDIATE,2,2)
      setOp(0x25,1,ZERO_PAGE,2,3)
      setOp(0x35,1,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x2D,1,ABSOLUTE,3,4)
      setOp(0x3D,1,ABSOLUTE_XINDEXED,3,4)
      setOp(0x39,1,ABSOLUTE_YINDEXED,3,4)
      setOp(0x21,1,XINDEXED_INDIRECT,2,6)
      setOp(0x31,1,INDIRECT_YINDEXED,2,5)

      // ASL:
      setOp(0x0A,2,ACCUMULATOR,1,2)
      setOp(0x06,2,ZERO_PAGE,2,5)
      setOp(0x16,2,ZERO_PAGE_XINDEXED,2,6)
      setOp(0x0E,2,ABSOLUTE,3,6)
      setOp(0x1E,2,ABSOLUTE_XINDEXED,3,7)

      // BCC:
      setOp(0x90,3,RELATIVE,2,2)

      // BCS:
      setOp(0xB0,4,RELATIVE,2,2)

      // BEQ:
      setOp(0xF0,5,RELATIVE,2,2)

      // BIT:
      setOp(0x24,6,ZERO_PAGE,2,3)
      setOp(0x2C,6,ABSOLUTE,3,4)

      // BMI:
      setOp(0x30,7,RELATIVE,2,2)

      // BNE:
      setOp(0xD0,8,RELATIVE,2,2)

      // BPL:
      setOp(0x10,9,RELATIVE,2,2)

      // BRK:
      setOp(0x00,10,IMPLIED,1,7)

      // BVC:
      setOp(0x50,11,RELATIVE,2,2)

      // BVS:
      setOp(0x70,12,RELATIVE,2,2)

      // CLC:
      setOp(0x18,13,IMPLIED,1,2)

      // CLD:
      setOp(0xD8,14,IMPLIED,1,2)

      // CLI:
      setOp(0x58,15,IMPLIED,1,2)

      // CLV:
      setOp(0xB8,16,IMPLIED,1,2)

      // CMP:
      setOp(0xC9,17,IMMEDIATE,2,2)
      setOp(0xC5,17,ZERO_PAGE,2,3)
      setOp(0xD5,17,ZERO_PAGE_XINDEXED,2,4)
      setOp(0xCD,17,ABSOLUTE,3,4)
      setOp(0xDD,17,ABSOLUTE_XINDEXED,3,4)
      setOp(0xD9,17,ABSOLUTE_YINDEXED,3,4)
      setOp(0xC1,17,XINDEXED_INDIRECT,2,6)
      setOp(0xD1,17,INDIRECT_YINDEXED,2,5)

      // CPX:
      setOp(0xE0,18,IMMEDIATE,2,2)
      setOp(0xE4,18,ZERO_PAGE,2,3)
      setOp(0xEC,18,ABSOLUTE,3,4)

      // CPY:
      setOp(0xC0,19,IMMEDIATE,2,2)
      setOp(0xC4,19,ZERO_PAGE,2,3)
      setOp(0xCC,19,ABSOLUTE,3,4)

      // DEC:
      setOp(0xC6,20,ZERO_PAGE,2,5)
      setOp(0xD6,20,ZERO_PAGE_XINDEXED,2,6)
      setOp(0xCE,20,ABSOLUTE,3,6)
      setOp(0xDE,20,ABSOLUTE_XINDEXED,3,7)

      // DEX:
      setOp(0xCA,21,IMPLIED,1,2)

      // DEY:
      setOp(0x88,22,IMPLIED,1,2)

      // EOR:
      setOp(0x49,23,IMMEDIATE,2,2)
      setOp(0x45,23,ZERO_PAGE,2,3)
      setOp(0x55,23,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x4D,23,ABSOLUTE,3,4)
      setOp(0x5D,23,ABSOLUTE_XINDEXED,3,4)
      setOp(0x59,23,ABSOLUTE_YINDEXED,3,4)
      setOp(0x41,23,XINDEXED_INDIRECT,2,6)
      setOp(0x51,23,INDIRECT_YINDEXED,2,5)

      // INC:
      setOp(0xE6,24,ZERO_PAGE,2,5)
      setOp(0xF6,24,ZERO_PAGE_XINDEXED,2,6)
      setOp(0xEE,24,ABSOLUTE,3,6)
      setOp(0xFE,24,ABSOLUTE_XINDEXED,3,7)

      // INX:
      setOp(0xE8,25,IMPLIED,1,2)

      // INY:
      setOp(0xC8,26,IMPLIED,1,2)

      // JMP:
      setOp(0x4C,27,ABSOLUTE,3,3)
      setOp(0x6C,27,INDIRECT,3,5)

      // JSR:
      setOp(0x20,28,ABSOLUTE,3,6)

      // LDA:
      setOp(0xA9,29,IMMEDIATE,2,2)
      setOp(0xA5,29,ZERO_PAGE,2,3)
      setOp(0xB5,29,ZERO_PAGE_XINDEXED,2,4)
      setOp(0xAD,29,ABSOLUTE,3,4)
      setOp(0xBD,29,ABSOLUTE_XINDEXED,3,4)
      setOp(0xB9,29,ABSOLUTE_YINDEXED,3,4)
      setOp(0xA1,29,XINDEXED_INDIRECT,2,6)
      setOp(0xB1,29,INDIRECT_YINDEXED,2,5)


      // LDX:
      setOp(0xA2,30,IMMEDIATE,2,2)
      setOp(0xA6,30,ZERO_PAGE,2,3)
      setOp(0xB6,30,ZERO_PAGE_YINDEXED,2,4)
      setOp(0xAE,30,ABSOLUTE,3,4)
      setOp(0xBE,30,ABSOLUTE_YINDEXED,3,4)

      // LDY:
      setOp(0xA0,31,IMMEDIATE,2,2)
      setOp(0xA4,31,ZERO_PAGE,2,3)
      setOp(0xB4,31,ZERO_PAGE_XINDEXED,2,4)
      setOp(0xAC,31,ABSOLUTE,3,4)
      setOp(0xBC,31,ABSOLUTE_XINDEXED,3,4)

      // LSR:
      setOp(0x4A,32,ACCUMULATOR,1,2)
      setOp(0x46,32,ZERO_PAGE,2,5)
      setOp(0x56,32,ZERO_PAGE_XINDEXED,2,6)
      setOp(0x4E,32,ABSOLUTE,3,6)
      setOp(0x5E,32,ABSOLUTE_XINDEXED,3,7)

      // NOP:
      setOp(0xEA,33,IMPLIED,1,2)

      // ORA:
      setOp(0x09,34,IMMEDIATE,2,2)
      setOp(0x05,34,ZERO_PAGE,2,3)
      setOp(0x15,34,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x0D,34,ABSOLUTE,3,4)
      setOp(0x1D,34,ABSOLUTE_XINDEXED,3,4)
      setOp(0x19,34,ABSOLUTE_YINDEXED,3,4)
      setOp(0x01,34,XINDEXED_INDIRECT,2,6)
      setOp(0x11,34,INDIRECT_YINDEXED,2,5)

      // PHA:
      setOp(0x48,35,IMPLIED,1,3)

      // PHP:
      setOp(0x08,36,IMPLIED,1,3)

      // PLA:
      setOp(0x68,37,IMPLIED,1,4)

      // PLP:
      setOp(0x28,38,IMPLIED,1,4)

      // ROL:
      setOp(0x2A,39,ACCUMULATOR,1,2)
      setOp(0x26,39,ZERO_PAGE,2,5)
      setOp(0x36,39,ZERO_PAGE_XINDEXED,2,6)
      setOp(0x2E,39,ABSOLUTE,3,6)
      setOp(0x3E,39,ABSOLUTE_XINDEXED,3,7)

      // ROR:
      setOp(0x6A,40,ACCUMULATOR,1,2)
      setOp(0x66,40,ZERO_PAGE,2,5)
      setOp(0x76,40,ZERO_PAGE_XINDEXED,2,6)
      setOp(0x6E,40,ABSOLUTE,3,6)
      setOp(0x7E,40,ABSOLUTE_XINDEXED,3,7)

      // RTI:
      setOp(0x40,41,IMPLIED,1,6)

      // RTS:
      setOp(0x60,42,IMPLIED,1,6)

      // SBC:
      setOp(0xE9,43,IMMEDIATE,2,2)
      setOp(0xE5,43,ZERO_PAGE,2,3)
      setOp(0xF5,43,ZERO_PAGE_XINDEXED,2,4)
      setOp(0xED,43,ABSOLUTE,3,4)
      setOp(0xFD,43,ABSOLUTE_XINDEXED,3,4)
      setOp(0xF9,43,ABSOLUTE_YINDEXED,3,4)
      setOp(0xE1,43,XINDEXED_INDIRECT,2,6)
      setOp(0xF1,43,INDIRECT_YINDEXED,2,5)

      // SEC:
      setOp(0x38,44,IMPLIED,1,2)

      // SED:
      setOp(0xF8,45,IMPLIED,1,2)

      // SEI:
      setOp(0x78,46,IMPLIED,1,2)

      // STA:
      setOp(0x85,47,ZERO_PAGE,2,3)
      setOp(0x95,47,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x8D,47,ABSOLUTE,3,4)
      setOp(0x9D,47,ABSOLUTE_XINDEXED,3,5)
      setOp(0x99,47,ABSOLUTE_YINDEXED,3,5)
      setOp(0x81,47,XINDEXED_INDIRECT,2,6)
      setOp(0x91,47,INDIRECT_YINDEXED,2,6)

      // STX:
      setOp(0x86,48,ZERO_PAGE,2,3)
      setOp(0x96,48,ZERO_PAGE_YINDEXED,2,4)
      setOp(0x8E,48,ABSOLUTE,3,4)

      // STY:
      setOp(0x84,49,ZERO_PAGE,2,3)
      setOp(0x94,49,ZERO_PAGE_XINDEXED,2,4)
      setOp(0x8C,49,ABSOLUTE,3,4)

      // TAX:
      setOp(0xAA,50,IMPLIED,1,2)

      // TAY:
      setOp(0xA8,51,IMPLIED,1,2)

      // TSX:
      setOp(0xBA,52,IMPLIED,1,2)

      // TXA:
      setOp(0x8A,53,IMPLIED,1,2)

      // TXS:
      setOp(0x9A,54,IMPLIED,1,2)

      // TYA:
      setOp(0x98,55,IMPLIED,1,2)
    }
  }
}
