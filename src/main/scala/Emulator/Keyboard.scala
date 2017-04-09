package Emulator

/** This class allows to map keyboard events (= button pressing) to specific instructions.
  * Will also allow to adapt to different keyboard types.
  */
class Keyboard() {

  //Buttons state variables
  var state1 = Array.fill[Int](8)(0x40)
  var state2 = Array.fill[Int](8)(0x40)
}