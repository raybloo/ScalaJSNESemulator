package Emulator

/** TODO : Class explanation
  * 
  */
class Program(nes: NES) {

  def load(data: Int): Unit = {

  }
  def getPrgRom(): ROM = {
    new ROM(nes)
  }
}
