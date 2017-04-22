package Emulator

import scala.scalajs.js
import js.timers
import scala.scalajs.js.timers.SetIntervalHandle

/** Class permitting to initialise, start and reset the emulator. 
  * Object instance will be called and used by almost all other classes.
  */
class NES() {
  // Init. all instances and state variables used for the emulator
  private val ui: UI = new UI
  private var program: Program = null

  // Init. accessible components
  var cpu: CPU = new CPU(this)
  var rom: ROM = null
  var keyboard: Keyboard = null //I'll see later how to implement this one
  var mmap: Mapper = null
  var ppu: PPU = new PPU(this)
  var papu: PAPU = new PAPU

  // Init. all default emulator value
  private var frameRate: Double = 60.0
  private var frameTime: Double = 1000.0/frameRate
  private var frameCount: Int = 0
  private var fpsInterval: Double = 500.0
  private var fpsLastTime: Double = 0.0
  private var intervalFpsDisplay: SetIntervalHandle = null
  private var intervalFrame: SetIntervalHandle = null
  private var isRunning: Boolean = false
  private var emulateSound: Boolean = true
  private var showDisplay: Boolean = true
  private var oldRomUrl: String = ""

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
        intervalFrame = timers.setInterval(frameTime) {frame}
        resetFps
        printFps
        intervalFpsDisplay = timers.setInterval(fpsInterval) {printFps}
      }
    } else {
      ui.updateStatus("Cannot start emulator: No ROM loaded")
    }
  }

  /** Simulates one frame of the nes*/
  def frame: Unit = {
    var cycles: Int = 0
    var stop: Boolean = false
    ppu.startFrame()
    //cpu.emulateCycle()
    while(!stop) {
      if (cpu.cyclesToHalt == 0) {
        // Execute a CPU instruction
        cycles = cpu.emulate()
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

  /** Stop the emulator */
  def stop: Unit = {
    timers.clearInterval(intervalFrame)
    timers.clearInterval(intervalFrame)
    isRunning = false
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

  /** Loads ROM into the ppu and the cpu*/
  def loadRom(romUrl: String): Boolean = {
    if (isRunning) {
      stop
    }
    ui.updateStatus("Loading ROM...")
    // Load ROM file:
    rom = new ROM(this)
    rom.openRom(romUrl)
    if(rom.checkRom) {
      reset
      mmap = rom.createMapper
      if (mmap == null) {
        false
      } else {
        mmap.loadROM
        //ppu.setMirroring(rom.getMirroringType)
        oldRomUrl = romUrl
        ui.updateStatus("Successfully loaded. Ready to be started.")
        true
      }
    }
    else {
      ui.updateStatus("Invalid ROM!")
      true
    }
  }

  def reloadRom: Unit = {
    if (oldRomUrl != null) {
      this.loadRom(oldRomUrl)
    }
  }


  /** Print the number of frame displayed per second */
  def printFps: Unit = {
    val now: Double = js.Date.now()
    var s: String = "Running"
    if (fpsLastTime != 0) {
      s += ": "+(frameCount.asInstanceOf[Double] / ((now - fpsLastTime) / 1000.0))+" FPS"
    }
    ui.updateStatus(s)
    frameCount = 0
    fpsLastTime = now
  }

  /** Reset frame per sec. counter */
  def resetFps: Unit = {
    fpsLastTime = 0
    frameCount = 0
  }

  /** Set a new preferred framerate */
  def setFramerate(rate: Double): Unit = {
    frameRate = rate
    frameTime = 1000 / frameRate
    //TODO when papu is functionnal
    //papu.setSampleRate(sampleRate, false)
  }
}
