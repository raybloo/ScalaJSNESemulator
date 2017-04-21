package Emulator

/** Picture Processing Unit class, permits to generate the images/video for the emulator.
  * Uses registers, a memory map, palettes, sprites and several tables.
  */
class PPU {


  var vramMem: Array[Byte] = null
  var spriteMem = null
  var vramAddress = null
  var vramTmpAddress = null
  var vramBufferedReadValue = null
  var firstWrite = null
  var sramAddress = null
  var currentMirroring = null
  var requestEndFrame: Boolean = false
  var nmiOk = null
  var dummyCycleToggle = null
  var nmiCounter: Int = 0
  var scanlineAlreadyRendered = null
  var f_nmiOnVblank = null
  var f_spriteSize = null
  var f_bgPatternTable = null
  var f_spPatternTable = null
  var f_addrInc = null
  var f_nTblAddress = null
  var f_color = null
  var f_spVisibility = null
  var f_bgVisibility = null
  var f_spClipping = null
  var f_bgClipping = null
  var f_dispType = null
  var cntFV = null
  var cntV = null
  var cntH = null
  var cntVT = null
  var cntHT = null
  var regFV = null
  var regV = null
  var regH = null
  var regVT = null
  var regHT = null
  var regFH = null
  var regS = null
  var curNt = null
  var attrib = null
  var buffer: Array[Int] = null
  var prevBuffer = null
  var bgbuffer = null
  var pixrendered = null

  var validTileData = null
  var scantile = null
  var scanline: Int = 0
  var lastRenderedScanline = null
  var curX: Int = 0
  var sprX = null
  var sprY = null
  var sprTile = null
  var sprCol = null
  var vertFlip = null
  var horiFlip = null
  var bgPriority = null
  var spr0HitX = null
  var spr0HitY = null
  var hitSpr0 = null
  var sprPalette = null
  var imgPalette = null
  var ptTile: Array[Tile] = null
  var ntable1 = null
  var nameTable = null
  var vramMirrorTable = null
  var palTable = null

  // Rendering Options:
  var showSpr0Hit: Boolean = false
  var clipToTvSize: Boolean = true

  def reset: Unit = {

  }

  def startVBlank: Unit = {

  }

  def endScanline: Unit = {

  }

  def startFrame(): Unit = {

  }

  def setStatusFlag(flag: Int,value: Boolean): Unit = {

  }



}

class Tile {

}
