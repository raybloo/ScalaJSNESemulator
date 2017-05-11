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

  val root: Div = dom.document.createElement("div").asInstanceOf[Div]
  val screen: Canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
  val romSelect: Input = dom.document.createElement("input").asInstanceOf[Input]
  val status: Paragraph = dom.document.createElement("paragraph").asInstanceOf[Paragraph]
  val startButton: Button = dom.document.createElement("button").asInstanceOf[Button]
  //TODO Buttons

  var zoomed: Boolean = false
  val ctx: dom.CanvasRenderingContext2D = screen.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  var canvasImageData: ImageData = ctx.getImageData(0,0,256,240)

  root.appendChild(screen)
  root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
  root.appendChild(status)
  root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
  root.appendChild(romSelect)
  root.appendChild(dom.document.createElement("BR").asInstanceOf[BR])
  root.appendChild(startButton)
  parent.appendChild(root)

  status.innerHTML = "No ROM loaded"

  screen.width = 256
  screen.height = 240

  romSelect.textContent = "https://raw.githubusercontent.com/raybloo/ScalaJSNESemulator/master/tetr.nes"
  romSelect.size = 300



  val startCallBack: Function[MouseEvent,_] = {
    e: MouseEvent =>
      nes.start
  }

  val loadCallBack: Function[MouseEvent,_] = {
    e: MouseEvent =>
      loadROM
      startButton.onclick = startCallBack
      startButton.innerHTML = "Start!"
  }

  startButton.innerHTML = "Load ROM"
  startButton.onclick = loadCallBack
//  romSelect.onchange {
//    case Event => nes.loadRom(romSelect.innerHTML)
//    case _ =>
//  }

  resetCanvas

  def main: Unit = {

  }

  /** Loads ROM */
  def loadROM: Unit = {
    nes.loadRom(romSelect.textContent)
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
    status.innerHTML = message
  }
  
  def writeFrame(buffer: Array[Int], prevBuffer: Array[Int]): Unit = {
    val imageData = canvasImageData.data
    var pixel: Int = 0
    for (i <- 0 to (256*240-1)) {
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
