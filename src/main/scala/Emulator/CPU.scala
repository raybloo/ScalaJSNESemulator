package Emulator

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
  var irqType: Byte = 3
  /* irqType are:
    - 0 : normal
    - 1 : nmi
    - 2 : reset
    - 3 : undefined
   */

 //Maskable Interrupt
  var interrupt = null

  //Addressing Modes
  val ZERO_PAGE: Byte = 0
  val INDEXED_ZERO_PAGE_X: Byte = 1
  val INDEXED_ZERO_PAGE_Y: Byte = 2
  val ABSOLUTE: Byte = 3
  val INDEXED_ABSOLUTE_X: Byte = 4
  val INDEXED_ABSOLUTE_Y: Byte = 5
  val IMPLIED: Byte = 6
  val ACCUMULATOR: Byte = 7
  val IMMEDIATE: Byte = 8
  val RELATIVE: Byte = 9
  val INDEXED_INDIRECT: Byte = 10
  val INDIRECT_INDEXED: Byte = 11
  val INDIRECT: Byte = 12


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
  def load1Word(address: Short): Byte = {
    if (address < 0x2000) {
      memory(address & 0x7ff)
    }
    else {
      nes.mmap.load(address)
    }
  }

  /** Load 2 bytes from memory */
  def load2Words(address: Short): Short = {
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




  def emulateCycle(): Int = {
    0
  }

}
