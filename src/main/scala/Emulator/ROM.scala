package Emulator

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}


class ROM {
  var is: InputStream = null

  def openRom(string: String): Unit = {
    //is = Files.newInputStream(Paths.get(string))
  }

  def getHeader(): Array[Byte] = {
    val bytes: Array[Byte] = new Array[Byte](16)
    is.reset()
    is.read(bytes,0,16)
    bytes
  }


}
