package Emulator

/** TODO : Class explanation
  * NB : Is a Mappers equivalent to the JNES architecture.
  */
class Program(nes: NES) {

  def load(data: Int): Unit = {

  }
  def getPrgRom(): ROM = {
    new ROM()
  }
}
