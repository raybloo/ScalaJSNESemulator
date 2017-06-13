package Emulator

import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}
import org.scalajs.dom.html._
import org.scalajs.dom.raw.{Event, ImageData}
import org.scalajs.jquery.jQuery

import scala.scalajs.js.JSApp

/** This class interfaces between the nes itself
  * and the html page it is rendered on. It acts
  * both as a screen and outputs sound for it.
  */
class UI(nes: NES) {
  val parent = dom.document.body

  var root: Div = null
  var screen: Canvas = null
  var romSelect: Input = null
  var status: Paragraph = null
  var startButton: Button = null
  //TODO Control Buttons

  var zoomed: Boolean = false
  var ctx: dom.CanvasRenderingContext2D = null
  var canvasImageData: ImageData = null

  var startCallBack: Function[MouseEvent,_] = _
  var stopCallBack: Function[MouseEvent,_] = _
  var loadCallBack: Function[MouseEvent,_] = _

  init //Comment this if you want to test the code

  def init: Unit = {

    root = dom.document.createElement("div").asInstanceOf[Div]
    screen = dom.document.createElement("canvas").asInstanceOf[Canvas]
    romSelect = dom.document.createElement("input").asInstanceOf[Input]
    status = dom.document.createElement("paragraph").asInstanceOf[Paragraph]
    startButton = dom.document.createElement("button").asInstanceOf[Button]

    zoomed = false
    ctx = screen.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    canvasImageData = ctx.getImageData(0,0,256,240)

    status.innerHTML = "No ROM loaded"

    screen.width = 256
    screen.height = 240

    romSelect.value = "https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/color_test.nes"
    romSelect.size = 300

    startCallBack = {
      e: MouseEvent =>
        nes.start
        startButton.onclick = stopCallBack
        startButton.innerHTML = "Stop"
    }

    stopCallBack = {
      e: MouseEvent =>
        nes.stop
        startButton.onclick = startCallBack
        startButton.innerHTML = "Start!"
    }

    loadCallBack = {
      e: MouseEvent =>
        loadROM
        startButton.onclick = startCallBack
        startButton.innerHTML = "Start!"
    }

    startButton.innerHTML = "Load ROM"
    startButton.onclick = loadCallBack

    root.appendChild(screen)
    root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
    root.appendChild(status)
    root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
    root.appendChild(romSelect)
    root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
    root.appendChild(startButton)
    parent.appendChild(root)

    resetCanvas
  }







//  romSelect.onchange {
//    case Event => nes.loadRom(romSelect.innerHTML)
//    case _ =>
//  }



  def main: Unit = {

  }

  /** Loads ROM */
  def loadROM: Unit = {
    nes.loadRom(romSelect.value)
  }

  /** Set the screen back to black with no transparency */
  def resetCanvas: Unit = {
    ctx.fillStyle = "black"
    // set alpha to opaque
    ctx.fillRect(0, 0, 256, 240)

    // Set alpha
    for( i <- (3 to canvasImageData.data.length-4) by 4) {
      canvasImageData.data(i) = 0xFF
    }
  }

  /** Return data url of what is currently on nes screen */
  def screenshot: String = {
    screen.toDataURL("image/png")
  }

  /** Enables buttons functionnalities */
  def enable: Unit = {
    //TODO
  }

  /** Change status message */
  def updateStatus(message: String): Unit = {
    if(status != null) status.innerHTML = message
  }

  def writeFrame(buffer: Array[Int], prevBuffer: Array[Int]): Unit = {
    val imageData = canvasImageData.data
    var pixel: Int = 0
    for (i <- 0 until 256*240) {
      pixel = buffer(i)
      if (pixel != prevBuffer(i)) {
        var j = i*4
        imageData(j) = pixel & 0xFF
        imageData(j+1) = (pixel >> 8) & 0xFF
        imageData(j+2) = (pixel >> 16) & 0xFF
        prevBuffer(i) = pixel
      }
    }
    ctx.putImageData(canvasImageData, 0, 0)
  }





}
