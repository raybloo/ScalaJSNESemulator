package Emulator

/**
  * Created by trolo on 15.03.2017.
  */
class CPU(nes: NES) {

  //Memory
  var memory = new Array[Byte](0x10000)

  //Program Counter (16 bits)
  var pc: Short = 0

  //Stack Pointer (8 bits)
  var sp: Byte = 0

  //Accumulator (8 bits)
  var a: Byte = 0

  //Index Register X (8 bits)
  var x: Byte = 0

  //Index Register Y (8 bits)
  var y: Byte = 0

  //Processor Status (8 bits)
  var p: Byte = 0

  //Processor Status parts, separated for convenience
  var carryFlag: Boolean = false //0th bit
  var zeroFlag: Boolean = true //1st bit
  var interruptDisable: Boolean = true //2nd bit
  var decimalModeFlag: Boolean = false //3rd bit
  var breakCommand: Boolean = false //4th bit
  //5th bit is unused
  var overflowFlag: Boolean = false //6th bit
  var negativeFlag: Boolean = false //7th bit

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


  def reset(): Unit = {
    memory = new Array[Byte](0x10000)
    carryFlag = false
    zeroFlag = false
    interruptDisable = true
    decimalModeFlag = false
    breakCommand = false
    overflowFlag = false
    negativeFlag = false

    var programRom = nes.program.getPrgRom()

    //Internal RAM
    for(i <- 0 to 0x2000) {
      memory(i) =  -1 //0xFF
    }

    //Everything else set to 0
    for(i <- 0x2000 to 0x8000) {
      memory(i) =  0 //0x00
    }

    pc = getResetVector()

    sp = -3 //0xFD

    a = 0
    x = 0
    y = 0

    p = getProcessorFlags()

  }

  //Find where the program begins
  def getResetVector(): Short = {
    loadMemory(0xFFfc,true)
  }

  //Get all the flags into one single Byte
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
    if(overflowFlag) {
      flags = (flags + 64).toByte
    }
    if(negativeFlag) {
      flags = (flags + 128).toByte
    }
    flags
  }

  //Load memory, with an optional double read for 2 byte
  def loadMemory(address: Short, doubleRead: Boolean): Short = {
    if(!doubleRead) {
      return 0;
    } else {
      return 0;
    }
  }

  def emulateCycle(): Unit = {

  }

}
