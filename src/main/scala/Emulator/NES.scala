package Emulator

/** Class permitting to initialise, start and reset the emulator. 
  * Object instance will be called and used by almost all other classes.
  */
class NES() {
  // Init. all instances and state variables used for the emulator
  private var cpu: CPU = new CPU(this)
  private var ppu: PPU = new PPU
  private var papu: PAPU = new PAPU
  private var romData: ROM = null
  private val ui: UI = new UI
  private var keyboard = ???
  private var mmap = ??? //I'll see later how to implement these 2
  private var program: Program = null
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

  def emulateCycle: Unit = {
    cpu.emulateCycle()
  }

  def reset(): Unit = {
    cpu.reset
    ppu.reset
    papu.reset
  }


}
