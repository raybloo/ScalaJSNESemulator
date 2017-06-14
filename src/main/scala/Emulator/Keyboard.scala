package Emulator

import scala.annotation.switch
import org.scalajs.dom

/** This class allows to map keyboard events (= button pressing) to specific instructions.
  * Will also allow to adapt to different keyboard types.
  */
class Keyboard() {

  //Buttons state variables
  var state1 = Array.fill[Int](8)(0x40)
  var state2 = Array.fill[Int](8)(0x40)

  final val KEY_A = 0
  final val KEY_B = 1
  final val KEY_SELECT = 2
  final val KEY_START = 3
  final val KEY_UP = 4
  final val KEY_DOWN = 5
  final val KEY_LEFT = 6
  final val KEY_RIGHT = 7

  def setKey(key: Int, value: Int): Boolean = {
    (key: @switch) match {
      case 88 => state1(KEY_A) = value      // X
      case 89 => state1(KEY_B) = value      // Y
      case 90 => state1(KEY_B) = value      // Z (American keyboard)
      case 17 => state1(KEY_SELECT) = value // Right Ctrl
      case 13 => state1(KEY_START) = value  // Enter
      case 38 => state1(KEY_UP) = value     // Up
      case 40 => state1(KEY_DOWN) = value   // Down
      case 37 => state1(KEY_LEFT) = value   // Left
      case 39 => state1(KEY_RIGHT) = value  // Right

      case 103 => state2(KEY_A) = value     // Num-7
      case 105 => state2(KEY_B) = value     // Num-9
      case 99 => state2(KEY_SELECT) = value // Num-3
      case 97 => state2(KEY_START) = value  // Num-1
      case 104 => state2(KEY_UP) = value    // Num-8
      case 98 => state2(KEY_DOWN) = value   // Num-2
      case 100 => state2(KEY_LEFT) = value  // Num-4
      case 102 => state2(KEY_RIGHT) = value // Num-6
      case _ => return true
    }
    false
  }

  def keyDown(evt: dom.KeyboardEvent): Unit = {
    if (!setKey(evt.keyCode, 0x41)) {
      evt.preventDefault
    }
  }

  def keyUp(evt: dom.KeyboardEvent): Unit = {
    if (!setKey(evt.keyCode, 0x40)) {
      evt.preventDefault()
    }
  }

  def keyPress(evt: dom.KeyboardEvent): Unit = {
    evt.preventDefault()
  }
}