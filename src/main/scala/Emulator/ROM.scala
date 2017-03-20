package Emulator


import scala.language.dynamics
import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.jquery.{JQueryAjaxSettings, JQueryXHR, jQuery}

import scala.scalajs.js.typedarray.{Int8Array,TA2AB}

/** TODO : Class explanation
  * 
  */
class ROM {
  var fullRom: Array[Byte] = null;
  var trainer: Array[Byte] = null;
  var prgRom: Array[Byte] = null;
  var chrRom: Array[Byte] = null;
  var PCPRom: Array[Byte] = null;
  var PCINSTRom: Array[Byte] = null;

  //Returns the header of the rom in the form of a byte array
  def getHeader(): Array[Byte] = {
    fullRom.slice(0,16)
  }

  //Returns the trainer, if has one, of the rom in the form of a byte array
  def getTrainer(): Array[Byte] = {
    if(hasTrainer()) {
      fullRom.slice(16,528)
    } else {
      null
    }
  }

  //Returns the prgRom of the rom in the form of a byte array
  def getPrgRom(): Array[Byte] = {
    val size = getPrgRomSize()
    if(hasTrainer()) {
      fullRom.slice(528,528+(16384*size))
    } else {
      fullRom.slice(16,16+(16384*size))
    }
  }

  //Returns the chrRom of the rom in the form of a byte array
  def getChrRom(): Array[Byte] = {
    var offset = 16;
    val size = getChrRomSize()
    if(hasTrainer()) offset += 512
    offset += 16384*((getHeader()(4).toInt + 256) % 256)
    fullRom.slice(offset,offset+(8196*size))
  }

  //Returns true when the rom has the 512 bytes of trainer before the prgrom
  def hasTrainer(): Boolean = {
    (getHeader()(6) & 4) != 0
  }

  //Returns the size in 16KB of the prgrom
  def getPrgRomSize(): Int = {
    // Scala Bytes are signed, but we want it unsigned, so we ll be using the modulo operator
    (getHeader()(4).toInt + 256) % 256 // conversion from signed to unsigned
  }

  //Returns the size in 8KB of the chrrom
  def getChrRomSize(): Int = {
    (getHeader()(5).toInt + 256) % 256 // conversion from signed to unsigned
  }

  //Returns the mirroring type as an int,
  // 0 being horizontal mirroring
  // 1 being vertical mirroring
  // 2 being fourscreen mirroring
  def getMirroringType(): Int = {
    val flagByte = getHeader()(6)
    if((flagByte & 8) != 0){
      2
    } else if((flagByte & 1) != 0) {
      1
    } else {
      0
    }
  }

  //Loads the requested rom into an array of byte, while performing some checks on the header
  def openRom(string: String): Unit = {
    //ajax request to get the ROM in a byte array format
    jQuery.ajax(
      js.Dynamic.literal(
        url = string,
        success = { (data: js.Any, textStatus: String, jqXHR: JQueryXHR) =>

          fullRom = data.toString.toCharArray.map(x => x.toByte)
          // This is the only way I could find to convert data into a scala.Array[Byte]

          if(checkRom()) {//performs a check before validating
            global.console.log(s"Successfully loaded $string")
          } else {
            global.console.log("File is not a valid ROM")
          }
        },
        error = { (jqXHR: JQueryXHR, textStatus: String, errorThrow: String) =>
          global.console.log(s"jqXHR=$jqXHR,text=$textStatus,err=$errorThrow")
        },
        `type` = "GET"
      ).asInstanceOf[org.scalajs.jquery.JQueryAjaxSettings])
  }

  //Checks if the fullROM meets the .nes header requirement
  def checkRom(): Boolean = {
    val header = getHeader()
    header(0) == 'N' &&
      header(1) == 'E' &&
      header(2) == 'S' &&
      header(10) == 0 &&
      header(11) == 0 &&
      header(12) == 0 &&
      header(13) == 0 &&
      header(14) == 0 &&
      header(15) == 0
  }



}
