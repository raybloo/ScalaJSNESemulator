package Emulator

import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import java.nio.file.{Path, Paths}

import scala.scalajs.js.JSApp

/** TODO : Class explanation
  * 
  */
object Main extends JSApp {
  val nes = new NES()
  var rom: ROM = null
  def main(): Unit = {
    rom = new ROM()
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    //jQuery("body").append("<p>Hello World</p>")
    jQuery("#click-me-button").click(loadRom _)
  }
  def loadRom(): Unit = {
    rom.openRom("c3.nes")
  }

  def addParagraph(string: String): Unit = {
    jQuery("body").append("<p>"+string+"<p>")
  }

}
