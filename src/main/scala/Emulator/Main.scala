package Emulator

import scala.scalajs.js
import js.{Dynamic, JSApp}
import js.annotation.JSExport
import org.scalajs.jquery.jQuery
import java.nio.file.{Path, Paths}

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global


/** TODO : Class explanation
  * 
  */
object Main extends JSApp {
  val nes = new NES()
  var rom: ROM = null

  def main(): Unit = {
    rom = new ROM(nes)
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    //jQuery("body").append("<p>Hello World</p>")
    jQuery("#click-me-button").click(loadRom _)
  }
  def loadRom(): Unit = {
    rom.openRom("https://gist.githubusercontent.com/yaotest/4064031/raw/5f1c56b9780eef54334726e9aaff70f105e615a8/test.txt")
    rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/c3.nes").onComplete {
      case Success(_) =>
        Dynamic.global.console.log(s"PrgRom Size is : ${rom.getPrgRomSize}")
        Dynamic.global.console.log(s"ChrRom Size is : ${rom.getChrRomSize}")
        Dynamic.global.console.log(s"Mapper name is : ${rom.getMapperName}")
      case Failure(e) => e.printStackTrace()
    }
    rom.openRom("https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/tetr.nes").onComplete {
      case Success(_) =>
        Dynamic.global.console.log(s"PrgRom Size is : ${rom.getPrgRomSize}")
        Dynamic.global.console.log(s"ChrRom Size is : ${rom.getChrRomSize}")
        Dynamic.global.console.log(s"Mapper name is : ${rom.getMapperName}")
      case Failure(e) => e.printStackTrace()
    }

  }

  def addParagraph(string: String): Unit = {
    jQuery("body").append("<p>"+string+"<p>")
  }

}
