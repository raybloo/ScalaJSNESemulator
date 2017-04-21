package Emulator

/** Picture Processing Unit class, permits to generate the images/video for the emulator.
  * Uses registers, a memory map, palettes, sprites and several tables.
  */
class PPU {

  /** The NameTable will contain two arrays:
    * - Tile:
    * 			Each byte in this array controls one 8x8 pixel character cell, and each nametable has 30 rows of 32 tiles each.
    *
    * - Attribute (Attrib):
    *			The attribute table is a 64-byte array at the end of each nametable that controls which palette is assigned to each part of the background. 
    *
    * A NES will contain 4 NameTables, arranged in a 2x2 pattern.
    */
  private class NameTable(w: Int, h: Int, n: String) {
    val width : Int = w
    val height : Int = h
    val name : String = n
	
    // Initially filled with 0's.
    var tile : Array[Int] = new Array(width*height)(0)
    /** Controls which palette is assigned to each part of the background.  */
    var attrib : Array[Int] = new Array(width*height)(0)
	
    /** Returns the searched tile. */
    def getTileIndex(x: Int, y: Int): Int = tile(y*width+x)
	
    /** Returns the searched attribute. */
    def getAttrib(x: Int, y: Int): Int = attrib(y*width+x)
	
    /** Writes given value in given index in the attributes table.
      * Note that the for loop (and bit operations) is necessary to select only what we want to change, as each byte in the attribute table controls a palette of a 32x32 pixel.
      */
    def writeAttrib(index: Int, value: Int): Unit = {
      var basex: Int = (index % 8) * 4
      var basey : Int = (scala.math.floor(index / 8) * 4).asInstanceOf[Int]
      var add : Int = 0
      var tx, ty : Int = 0
      var attindex : Int = 0
		
      var sqy, sqx : Int = 0
      for (sqy <- 1 to 2; sqx <- 1 to 2) {
        add = (value>>(2*(sqy*2+sqx)))&3 // Bit operators
        for (y <- 1 to 2; x <- 1 to 2) {
          tx = basex+sqx*2+x
          ty = basey+sqy*2+y
          attindex = ty*width+tx
          attrib(ty*width+tx) = (add<<2)&12
        }
      }
    }
	
    // TODO if necessary : To and from JSON
  }
  
  /** The PaletteTable will contain two arrays and one global variable:
    * - curTable:
    *     Contains the used color palette. Can be either the default NES, NTSC or PAL.
    *
    * - Emphasis Table (emphTable):
    *	    Will contain the corresponding color emphasis for each color of the palette.
    *
    * - Emphasis (currentEmph and emph):
    *      The color emphasis will be used to change the luminance amd chrominance phasor of each color, by using the RGB components.
    * 
    * Each NES has only one color Palette.
    */
  private class PaletteTable {
    var curTable : Array[Int] = new Array(64)
    var emphTable : Array[Array[Int]] = Array.ofDim[Int](8, 64)
    var currentEmph : Int = -1
	
    /** Resets the Palette */
    def reset(): Unit = setEmphasis(0)
	
    /** Loads the NTSC Palette */
    def loadNTSCPalette(): Unit = {
      curTable = Array(0x525252, 0xB40000, 0xA00000, 0xB1003D, 0x740069, 0x00005B, 0x00005F, 0x001840, 0x002F10, 0x084A08, 0x006700, 0x124200, 0x6D2800, 0x000000, 0x000000, 0x000000, 0xC4D5E7, 0xFF4000, 0xDC0E22, 0xFF476B, 0xD7009F, 0x680AD7, 0x0019BC, 0x0054B1, 0x006A5B, 0x008C03, 0x00AB00, 0x2C8800, 0xA47200, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0xFFAB3C, 0xFF7981, 0xFF5BC5, 0xFF48F2, 0xDF49FF, 0x476DFF, 0x00B4F7, 0x00E0FF, 0x00E375, 0x03F42B, 0x78B82E, 0xE5E218, 0x787878, 0x000000, 0x000000, 0xFFFFFF, 0xFFF2BE, 0xF8B8B8, 0xF8B8D8, 0xFFB6FF, 0xFFC3FF, 0xC7D1FF, 0x9ADAFF, 0x88EDF8, 0x83FFDD, 0xB8F8B8, 0xF5F8AC, 0xFFFFB0, 0xF8D8F8, 0x000000, 0x000000)
      makeTables()
      setEmphasis(0)
    }
	
    /** Loads the PAL Palette */
    def loadPALPalette(): Unit = {
      curTable = Array(0x525252, 0xB40000, 0xA00000, 0xB1003D, 0x740069, 0x00005B, 0x00005F, 0x001840, 0x002F10, 0x084A08, 0x006700, 0x124200, 0x6D2800, 0x000000, 0x000000, 0x000000, 0xC4D5E7, 0xFF4000, 0xDC0E22, 0xFF476B, 0xD7009F, 0x680AD7, 0x0019BC, 0x0054B1, 0x006A5B, 0x008C03, 0x00AB00, 0x2C8800, 0xA47200, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0xFFAB3C, 0xFF7981, 0xFF5BC5, 0xFF48F2, 0xDF49FF, 0x476DFF, 0x00B4F7, 0x00E0FF, 0x00E375, 0x03F42B, 0x78B82E, 0xE5E218, 0x787878, 0x000000, 0x000000, 0xFFFFFF, 0xFFF2BE, 0xF8B8B8, 0xF8B8D8, 0xFFB6FF, 0xFFC3FF, 0xC7D1FF, 0x9ADAFF, 0x88EDF8, 0x83FFDD, 0xB8F8B8, 0xF5F8AC, 0xFFFFB0, 0xF8D8F8, 0x000000, 0x000000)
      makeTables()
      setEmphasis(0);
    }
	
    /** Creates the emphasis table for each existing emphasis (0 to 8), using the given Palette (NTSC, PAL or default) */
    def makeTables(): Unit = {
      var r, g, b, col, i : Int = 0
      var rFactor, gFactor, bFactor : Double = 1.0
      
      // Calculate a table for each possible emphasis
      for (emph <- 1 to 8) {
        // Determine color componenet factors
        if ((emph & 1) != 0) {
          rFactor = 0.75
          bFactor = 0.75
        }
        if ((emph & 2) != 0) {
          rFactor = 0.75
          gFactor = 0.75
        }
        if ((emph & 4) != 0) {
          gFactor = 0.75
          bFactor = 0.75
        }
        
        // Calculate table
        for (i <- 1 to 64) {
          col = curTable(i)
          r = scala.math.floor(getRed(col) * rFactor).asInstanceOf[Int]
          g = scala.math.floor(getGreen(col) * gFactor).asInstanceOf[Int]
          b = scala.math.floor(getBlue(col) * bFactor).asInstanceOf[Int]
          emphTable(emph)(i) = getRgb(r,g,b)
        }
      }
    }
	
    /** Sets the current emphasis to given value. */
    def setEmphasis(emph: Int): Unit = {
      if (emph != currentEmph) {
        currentEmph = emph
        var i : Int = 0
        for (i <- 1 to 64) curTable(i) = emphTable(emph)(i)
      }
    }
	
    /** Get the given entry form the curTable */
    def getEntry(yiq: Int): Int = curTable(yiq)
	
    /** Extract red component from a RGB color */
    def getRed(rgb: Int): Int = (rgb>>16)&0xFF
	
    /** Extract green component from a RGB color */
    def getGreen(rgb: Int): Int = (rgb>>8)&0xFF
	
    /** Extract blue component from a RGB color */
    def getBlue(rgb: Int): Int = (rgb&0xFF)
	
    /** Get the RGB color, given the red, blue and green component */
    def getRgb(r: Int, g: Int, b: Int): Int = ((r<<16)|(g<<8)|(b))
	
    /** Loads the default NES Palette (given by wiki) */
    def loadDefaultPalette(): Unit = {
      curTable(0) = getRgb(117,117,117)
      curTable(1) = getRgb(39,27,143)
      curTable(2) = getRgb(0,0,171)
      curTable(3) = getRgb(71,0,159)
      curTable(4) = getRgb(143,0,119)
      curTable(5) = getRgb(171,0,19)
      curTable(6) = getRgb(167,0,0)
      curTable(7) = getRgb(127,11,0)
      curTable(8) = getRgb(67,47,0)
      curTable(9) = getRgb(0,71,0)
      curTable(10) = getRgb(0,81,0)
      curTable(11) = getRgb(0,63,23)
      curTable(12) = getRgb(27,63,95)
      curTable(13) = getRgb(0,0,0)
      curTable(14) = getRgb(0,0,0)
      curTable(15) = getRgb(0,0,0)
      curTable(16) = getRgb(188,188,188)
      curTable(17) = getRgb(0,115,239)
      curTable(18) = getRgb(35,59,239)
      curTable(19) = getRgb(131,0,243)
      curTable(20) = getRgb(191,0,191)
      curTable(21) = getRgb(231,0,91)
      curTable(22) = getRgb(219,43,0)
      curTable(23) = getRgb(203,79,15)
      curTable(24) = getRgb(139,115,0)
      curTable(25) = getRgb(0,151,0)
      curTable(26) = getRgb(0,171,0)
      curTable(27) = getRgb(0,147, 59)
      curTable(28) = getRgb(0,131,139)
      curTable(29) = getRgb(0,0,0)
      curTable(30) = getRgb(0,0,0)
      curTable(31) = getRgb(0,0,0)
      curTable(32) = getRgb(255,255,255)
      curTable(33) = getRgb(63,191,255)
      curTable(34) = getRgb(95,151,255)
      curTable(35) = getRgb(167,139,253)
      curTable(36) = getRgb(247,123,255)
      curTable(37) = getRgb(255,119,183)
      curTable(38) = getRgb(255,119,99)
      curTable(39) = getRgb(255,155,59)
      curTable(40) = getRgb(243,191,63)
      curTable(41) = getRgb(131,211,19)
      curTable(42) = getRgb(79,223,75)
      curTable(43) = getRgb(88,248,152)
      curTable(44) = getRgb(0,235,219)
      curTable(45) = getRgb(0,0,0)
      curTable(46) = getRgb(0,0,0)
      curTable(47) = getRgb(0,0,0)
      curTable(48) = getRgb(255,255,255)
      curTable(49) = getRgb(171,231,255)
      curTable(50) = getRgb(199,215,255)
      curTable(51) = getRgb(215,203,255)
      curTable(52) = getRgb(255,199,255)
      curTable(53) = getRgb(255,199,219)
      curTable(54) = getRgb(255,191,179)
      curTable(55) = getRgb(255,219,171)
      curTable(56) = getRgb(255,231,163)
      curTable(57) = getRgb(227,255,163)
      curTable(58) = getRgb(171,243,191)
      curTable(59) = getRgb(179,255,207)
      curTable(60) = getRgb(159,255,243)
      curTable(61) = getRgb(0,0,0)
      curTable(62) = getRgb(0,0,0)
      curTable(63) = getRgb(0,0,0)
        
      makeTables()
      setEmphasis(0)
    }
	
  }

  /** The Tile contains:
    * - pix and opaque:
    *     Array containing all the tile data and array containing the boolean value if a Tile is opaque or not
    *
    * - fbIndex, tIndex and palIndex
    *	    The frame buffer (Tile index in buffer), tile Index in pix and palette Index to be used on that Tile.
    *
    * - w and h
    *      Width and height of a tile
    *
    * - initialized
    *      Boolean true if scan line was set.
    *
    * - tpri
    *      Unknown for now
    * 
    * The Tile class will be used to render each tile in the buffer for the NES.
    */
  private class Tile {
    // Tile data:
    var pix: Array[Int] = new Array(64)
    
    // Frame buffer Index of a tile
    var fbIndex : Int = _
    // Tile Index in pix
    var tIndex : Int = _
    var w : Int = _
    var h : Int = _
    var palIndex : Int = _
    var tpri : Int = _
    var initialized : Boolean = false
    var opaque : Array[Boolean] = new Array(8)
    
    var srcx1 : Int = _
    var srcx2 : Int = _
    var srcy1 : Int = _
    var srcy2 : Int = _
  
  /** Will create the scanline */
    def setBuffer(scanlineArray : Array[Int]): Unit = {
      var y : Int = 0
      for (y <- 1 to 8) setScanline(y, scanlineArray(y), scanlineArray(y+8))
    }
  
    /** Sets the scanline, by initializing the pix data */
    def setScanline(sline: Int, b1: Int, b2: Int): Unit = {
      var initialized : Boolean = true
      var tIndex : Int = sline<<3
      var x : Int = 0
      
      for (x <- 1 to 8) {
        pix(tIndex + x) = ((b1 >> (7 - x)) & 1) + (((b2 >> (7 - x)) & 1) << 1)
        if (pix(tIndex + x) == 0) opaque(sline) = false
      }
    }
    
    /** Looks if a tile is visible or not */
    def isTransparent(x: Int, y: Int): Boolean = pix((y << 3) + x) == 0
  
    /**
      * Renders the tile inside the buffer using different values and conditions.
      *
      * Variables are :
      *   buffer : buffer.
      *   srcx & srcy : size of the sprite, usually 8x8, so scr_1 = 0 and src_2 = 8
      *   dx & dy : the first x and y coordinates of sprite data
      *   palAdd : Upper two bits of color
      *   palette :  shapes of tiles that make up backgrounds and sprites, palette data
      *   flipHorizontal : Horizontal flip of a sprite
      *   flipVertical : Vertical flip
      *   pritable : Unknown
      *   pri : Unknown
      */
    def render(buffer: Array[Int], srcx1v: Int, srcy1v: Int, srcx2v: Int, srcy2v: Int, dx: Int, dy: Int, palAdd: Int, palette: Array[Int], flipHorizontal: Boolean, flipVertical: Boolean, pri: Int, priTable: Array[Int]): Unit = {
      srcx1 = srcx1v
      srcx2 = srcx2v
      srcy1 = srcy1v
      srcy2 = srcy2v
      
      if (dx < -7 || dx >= 256 || dy < -7 || dy >= 240) return
      
      w = srcx2 - srcx1
      h = srcy2 - srcy1
      
      if (dx < 0) srcx1 -= dx
      if (dx + srcx2 >= 256) srcx2 = 256 - dx
      if (dy < 0) srcy1 -= dy
      if (dy + srcy2 >= 240) srcy2 = 240- dy
      
      fbIndex = (dy<<8) + dx
      
      if (!flipHorizontal && !flipVertical) { // Upright tile
        tIndex = 0
        for (y <- 1 to 8) {
          for (x <- 1 to 8) {
            // Code in if is the same everywhere. So I put it in a function
            renderFunction(buffer, palette, palAdd, x, y, pri, priTable)
            fbIndex += 1
            tIndex += 1
          }
          // Not sure why he first take 8 then adds 256 ? Kept it as is for now
          fbIndex -= 8
          fbIndex += 256
        }
        
      } else if (flipHorizontal && !flipVertical) { // Mirrored tile
        tIndex = 7
        for (y <- 1 to 8) {
          for (x <- 1 to 8) {
            // Code in if is the same everywhere. So I put it in a function
            renderFunction(buffer, palette, palAdd, x, y, pri, priTable)
            fbIndex += 1
            tIndex -= 1
          }
          fbIndex -= 8
          fbIndex += 256
          tIndex += 16
        }
        
      } else if(flipVertical && !flipHorizontal) {  // Reflected tile
        tIndex = 56
      
        for (y <- 1 to 8) {
          for (x <- 1 to 8) {
            // Code in if is the same everywhere. So I put it in a function
            renderFunction(buffer, palette, palAdd, x, y, pri, priTable)
            fbIndex += 1
            tIndex += 1
          }
          fbIndex -= 8
          fbIndex += 256
          tIndex -= 16
        }
        
      } else { // flipVertical && flipHorizontal. Inverted tile
        tIndex = 63
      
        for (y <- 1 to 8) {
          for (x <- 1 to 8) {
            // Code in if is the same everywhere. So I put it in a function
            renderFunction(buffer, palette, palAdd, x, y, pri, priTable)
            fbIndex += 1
            tIndex -= 1
          }
          fbIndex -= 8
          fbIndex += 256
        }
        
      }
      
    }
    
    /** Helper function that actively renders tile to buffer. Used to not have duplicate code. */
    def renderFunction(buffer: Array[Int], palette: Array[Int], palAdd: Int, x: Int, y: Int, pri: Int, priTable: Array[Int]): Unit = {
      if (x >= srcx1 && x < srcx2 && y >= srcy1 && y < srcy2) {
        palIndex = pix(tIndex)
        tpri = priTable(fbIndex)
        if (palIndex != 0 && pri <= (tpri&0xFF)) {
          // Rendering tile to buffer
          buffer(fbIndex) = palette(palIndex+palAdd)
          tpri = (tpri&0xF00)|pri
          priTable(fbIndex) = tpri
        }
      }
    }
  
    // TODO if necessary : To and from JSON
  }

  var vramMem = null
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
  var buffer = null
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
  var ptTile = null
  var ntable1 = null
  var nameTable = null
  var vramMirrorTable = null
  var palTable = null

  // Rendering Options:
  var showSpr0Hit: Boolean = false
  var clipToTvSize: Boolean = true

  // Status flags:
  val StatusVRAMWrite: Byte =  4
  val StatusSLSpriteCount: Byte =  5
  val StatusSprite0Hit: Byte =  6
  val StatusVBlank: Byte =  7
  
  def reset: Unit = {
    
  }
  
  def setMirroring(mirroring: Int): Unit = {

  }
  
  def defineMirrorRegion(fromStart: Int, toStart: Int, size: Int): Unit = {

  }

  def startVBlank: Unit = {

  }

  def endScanline: Unit = {

  }

  def startFrame(): Unit = {

  }
  
  def endFrame: Unit = {

  }
  
  def updateControlReg1(value: Int): Unit = {

  }
  
  def updateControlReg2(value: Int): Unit = {

  }

  def setStatusFlag(flag: Int, value: Boolean): Unit = {

  }
  
  def readStatusRegister: Unit = {

  }
  
  def writeSRAMAddress(address: Int): Unit = {

  }
  
  def sramLoad: Unit = {

  }
  
  def sramWrite(value: Int): Unit = {

  }
  
  def scrollWrite(value: Int): Unit = {

  }
  
  def writeVRAMAddress(address: Int): Unit = {

  }
  
  def vramLoad: Unit = {

  }
  
  def vramWrite(value: Int): Unit = {

  }
  
  def sramDMA(value: Int): Unit = {

  }
  
  def regsFromAddress: Unit = {

  }
  
  def cntsFromAddress: Unit = {

  }
  
  def regsToAddress: Unit = {

  }
  
  def cntsToAddress: Unit = {

  }
  
  def incTileCounter(count: Int): Unit = {

  }
  
  def mirroredLoad(address: Int): Unit = {

  }
  
  def mirroredWrite(address: Int, value: Int): Unit = {

  }
  
  def triggerRendering: Unit = {

  }
  
  def renderFramePartially(startScan: Int, scanCount: Int): Unit = {

  }
  
  def renderBgScanline(pbgbuffer: Boolean, scan: Int): Unit = {

  }
  
  def renderSpritesPartially(startScan: Int, scanCount: Int, bgPri: Int): Unit = {

  }
  
  def checkSprite0(scan: Int): Unit = {

  }
  
  def writeMem(address: Int, value: Int): Unit = {

  }
  
  def updatePalettes: Unit = {

  }
  
  def patternWrite(address: Int, value: Int): Unit = {

  }
  
  def nameTableWrite(index: Int, address: Int, value: Int): Unit = {

  }
  
  def attribTableWrite(index: Int, address: Int, value: Int): Unit = {

  }
  
  def spriteRamWriteUpdate(address: Int, value: Int): Unit = {

  }
  
  def doNMI: Unit = {

  }
  
  // TODO if needed to and from JSON

}
