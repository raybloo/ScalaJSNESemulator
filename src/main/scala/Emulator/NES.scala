package Emulator

import scala.scalajs.js
import js.{Dynamic, timers}
import scala.concurrent.Future
import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/** Class permitting to initialise, start and reset the emulator.
  * Object instance will be called and used by almost all other classes.
  */
class NES() {
  // Init. all instances and state variables used for the emulator
  val ui: UI = new UI(this)

  // Init. accessible components
  var cpu: CPU = new CPU(this)
  var rom: ROM = new ROM(this)
  var keyboard: Keyboard = new Keyboard
  var mmap: Mapper = null
  var ppu: PPU = new PPU(this)
  var papu: PAPU = new PAPU

  // Init. all default emulator value
  var frameRate: Double = 60.0
  var frameTime: Double = 1000.0/frameRate
  var frameCount: Int = 0
  var fpsInterval: Double = 500.0
  var fpsLastTime: Double = 0.0
  var intervalFpsDisplay: SetIntervalHandle = null
  var intervalFrame: SetIntervalHandle = null
  var isRunning: Boolean = false
  var emulateSound: Boolean = true
  var showDisplay: Boolean = true
  var oldRomUrl: String = ""
  var mapperTable: Array[() => Mapper] = (for(i <- 0 to 92) yield (() => new NoMapper(this))).toArray

  mapperTable(1) = (() => new MMC1(this))
  mapperTable(5) = (() => new MMC5(this))

  ui.updateStatus("Ready to load ROM")

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
        cycles = cpu.emulate
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
      while(cycles > 0 && !stop) {
        if(ppu.curX == ppu.spr0HitX &&
          ppu.f_spVisibility == 1 &&
          ppu.scanline - 21 == ppu.spr0HitY) {
          // Set sprite 0 hit flag:
          ppu.setStatusFlag(6, true) //6 stands for the sprite 0 hit flag
        }

        if (ppu.requestEndFrame) {
          ppu.nmiCounter -= 1
          if (ppu.nmiCounter == 0) {
            //Dynamic.global.console.log("End frame requested")
            ppu.requestEndFrame = false
            ppu.startVBlank
            stop = true
          }
        }

        if(!stop) {
          ppu.curX+=1
          if (ppu.curX == 341) {
            ppu.curX = 0
            ppu.endScanline
          }
          cycles -= 1
        }
      }
    }
    Dynamic.global.console.log(s"Frame Completed: ${cpu.instructionCounter}")
    frameCount += 1
  }

  /** Stop the emulator */
  def stop: Unit = {
    timers.clearInterval(intervalFrame)
    timers.clearInterval(intervalFpsDisplay)
    isRunning = false
    ui.updateStatus("Paused")
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
  def loadRom(romUrl: String): Unit = {
    if (isRunning) {
      stop
    }
    reset
    ui.updateStatus("Loading ROM...")
    // Load ROM file:
    rom = new ROM(this)
    val romLoading: Future[Any] = rom.openRom(romUrl)
    romLoading.onComplete {
      case Success(value) =>
        if(rom.checkRom) {
          mmap = rom.createMapper
          if (mmap == null) {
            //false
            ui.updateStatus("Mapper was not loaded correctly")
          } else {
            mmap.loadROM
            ppu.setMirroring(rom.getMirroringType)
            oldRomUrl = romUrl
            ui.updateStatus("Successfully loaded. Ready to be started.")
            //true
            Dynamic.global.console.log(s"PrgRom Size is : ${rom.getPrgRomSize}")
            Dynamic.global.console.log(s"ChrRom Size is : ${rom.getChrRomSize}")
            Dynamic.global.console.log(s"Mapper name is : ${rom.getMapperName}")
            Dynamic.global.console.log(s"Mirroring type is : ${rom.getMirroringType}")
            Dynamic.global.console.log(s"Nametable's content is : ${ppu.ntable1(0)}, ${ppu.ntable1(1)}, ${ppu.ntable1(2)}, ${ppu.ntable1(3)}")
          }
        }
        else {
          ui.updateStatus("Invalid ROM!")
          //true
        }

      case Failure(e) =>
        ui.updateStatus("Unable to load ROM")
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
