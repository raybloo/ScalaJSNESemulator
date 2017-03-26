package Emulator

import scala.scalajs.js
import js.timers.setInterval

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
  private var fpsInterval: Double = 500.0
  private var isRunning: Boolean = false
  private var emulateSound: Boolean = true
  private var showDisplay: Boolean = true

  ui.updateStatus("Ready to load ROM")

  def loadProgram(data: Int): Unit = {
    program = new Program(this)
    program.load(data)
    reset
  }

  /** Launch the emulator */
  def start: Unit = {
    if(rom != null) {
      if(!isRunning) {
        isRunning = true
        setInterval(frameTime) {frame}
        resetFps
        printFps
        setInterval(fpsInterval) {printFps}
      }
    } else {
      ui.updateStatus("Cannot start emulator: No ROM loaded")
    }
  }

  def frame: Unit = {
    var cycles: Int = 0
    var stop: Boolean = false
    ppu.startFrame()
    //cpu.emulateCycle()
    while(!stop) {
      if(cpu.cyclesToHalt > 8) {
        cycles = 24
        if(emulateSound) {
          //papu should
        }
      }
    }
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
