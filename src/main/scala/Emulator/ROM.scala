package Emulator


import scala.language.dynamics
import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.jquery.{JQueryAjaxSettings, JQueryXHR, jQuery}

import scala.reflect.ClassTag
import scala.scalajs.js.typedarray.{Int8Array,TA2AB}


class ROM {
  var fullRom: Array[Byte] = null;
  var prgRom: Array[Byte] = null;

  def getHeader(): Array[Byte] = {
    fullRom.slice(0,16)
  }

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
    header(2) == 'S'
    true
  }



}
