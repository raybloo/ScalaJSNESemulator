package Emulator

/** Class permitting to initialise, start and reset the emulator. 
  * Object instance will be called and used by almost all other classes.
  */
class NES() {
  // Init. all instances and state variables used for the emulator
  private var cpu: CPU = new CPU(this)
  private var ppu: PPU = new PPU
  private var papu: PAPU = new PAPU
  private var rom: ROM = null
  private val ui: UI = new UI
  private var keyboard = ??? //I'll see later how to implement this one
  private var mmap: Mapper = null
  private var program: Program = null

  // Init. all default emulator value
  private var frameRate: Double = 60.0
  private var frameTime: Double = 1000.0/frameRate
  private var isRunning: Boolean = false
  private var emulateSound: Boolean = true
  private var showDisplay: Boolean = true

  ui.updateStatus("Ready to load ROM")

  def loadProgram(data: Int): Unit = {
    program = new Program(this)
    program.load(data)
    reset()
  }

  /** Launch the emulator */
  def start: Unit = {
    if(rom != null) {
      if(!isRunning) {
        isRunning = true

      }
    } else {
      ui.updateStatus("Cannot start emulator: No ROM loaded")
    }
  }

  def emulateCycle: Unit = {
    cpu.emulateCycle()
  }

  /** Reset all components */
  def reset: Unit = {
    if(mmap != null) {
      mmap.reset
    }
    cpu.reset
    ppu.reset
    papu.reset
  }


  /** Print the number of frame displayed per second */
  def printFps: Unit = {

  }

  /** Reset frame per sec. counter */
  def resetFps: Unit = {

  }


}
