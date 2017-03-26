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
  var rom: ROM = null // rom will be used by other compenents therefore it is not private
  private val ui: UI = new UI
  private var keyboard = ??? //I'll see later how to implement this one
  private var mmap: Mapper = null
  private var program: Program = null

  // Init. all default emulator value
  private var frameRate: Double = 60.0
  private var frameTime: Double = 1000.0/frameRate
  private var fpsInterval: Double = 500.0
  private var frameCount: Int = 0
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
      if (cpu.cyclesToHalt == 0) {
        // Execute a CPU instruction
        cycles = cpu.emulateCycle()
        if(emulateSound) {
          //TODO implement when papu is functionnal
        }
        cycles *= 3
      }
      else {
        if(cpu.cyclesToHalt > 8) {
          cycles = 24
          if(emulateSound) {
            //TODO implement when papu is functionnal
          }
          cpu.cyclesToHalt -= 8
        }
        else {
          cycles = cpu.cyclesToHalt * 3
          if(emulateSound) {
            //TODO implement when papu is functionnal
          }
          cpu.cyclesToHalt = 0
        }
      }
      for (i <- cycles to 1) {
        if(ppu.curX == ppu.spr0HitX &&
          ppu.f_spVisibility == 1 &&
          ppu.scanline - 21 == ppu.spr0HitY) {
          // Set sprite 0 hit flag:
          ppu.setStatusFlag(6, true) //6 stands for the sprite 0
        }

        if (ppu.requestEndFrame) {
          ppu.nmiCounter -= 1
          if (ppu.nmiCounter == 0) {
            ppu.requestEndFrame = false
            ppu.startVBlank
            stop = true
          }
        }

        if(!stop) {
          ppu.curX
          if (ppu.curX == 341) {
            ppu.curX = 0
            ppu.endScanline
          }
        }
      }
      cycles = 0
    }
    frameCount += 1
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
