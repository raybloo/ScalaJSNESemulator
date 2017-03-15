package Emulator

/**
  * Created by trolo on 15.03.2017.
  */
class NES() {
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
