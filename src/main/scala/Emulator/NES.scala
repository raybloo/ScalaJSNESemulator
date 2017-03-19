package Emulator

/** Class permitting to initialise, start and reset the emulator. 
  * Object instance will be called and used by almost all other classes.
  */
class NES() {
  // Init. all instances used for the emulator
  var cpu: CPU = new CPU(this)
  var program: Program = null

  def loafProgram(data: Int): Unit = {
    program = new Program(this)
    program.load(data)
    reset()
  }

  def emulateCycle: Unit = {
    cpu.emulateCycle()
  }

  def reset(): Unit = {
    cpu.reset()
  }


}
