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
	
	// Initialised filled with 0's.
	var tile : Array[Byte] = new Array(width*height)(0)
	/** Controls which palette is assigned to each part of the background.  */
	var attrib : Array[Byte] = new Array(width*height)(0)
	
	/** Returns the searched tile. */
	def getTileIndex(x: Int, y: Int): Byte = {
		return tile(y*width+x)
	}
	
	/** Returns the searched attribute. */
	def getAttrib(x: Int, y: Int): Byte = {
		return attrib(y*width+x)
	}
	
	/** Writes given value in given index in the attributes table.
	  * Note that the for loop (and bit operations) is necessary to select only what we want to change, as each byte in the attribute table controls a palette of a 32x32 pixel.
	  */
	def writeAttrib(index: Int, value: Byte): Unit = {
		var basex: Int = (index % 8) * 4;
        var basey : Int = (scala.math.floor(index / 8) * 4).asInstanceOf[Int];
        var add : Byte = 0
        var tx, ty : Int = 0
		var attindex : Int = 0
		
		var sqy, sqx : Int = 0
		for (sqy <- 1 to 2; sqx <- 1 to 2) {
			add = (((value>>(2*(sqy*2+sqx))).asInstanceOf[Byte])&3).asInstanceOf[Byte]; // Bit operators
			for (y <- 1 to 2; x <- 1 to 2) {
				tx = basex+sqx*2+x;
                ty = basey+sqy*2+y;
                attindex = ty*width+tx;
                attrib(ty*width+tx) = ((add<<2)&12).asInstanceOf[Byte];
			}
		}
	
		
	}
	
    // TODO if necessary : To and from JSON
  }
  
  private class PaletteTable {
    var curTable : Array[Byte] = new Array(64)
	var emphTable : Array[Byte] = new Array(8)
	var currentEmph : Byte = -1
	
	def reset(): Unit = {
		setEmphasis(0)
	}
	
	def loadNTSCPalette(): Unit = {
		curTable = Array(0x525252, 0xB40000, 0xA00000, 0xB1003D, 0x740069, 0x00005B, 0x00005F, 0x001840, 0x002F10, 0x084A08, 0x006700, 0x124200, 0x6D2800, 0x000000, 0x000000, 0x000000, 0xC4D5E7, 0xFF4000, 0xDC0E22, 0xFF476B, 0xD7009F, 0x680AD7, 0x0019BC, 0x0054B1, 0x006A5B, 0x008C03, 0x00AB00, 0x2C8800, 0xA47200, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0xFFAB3C, 0xFF7981, 0xFF5BC5, 0xFF48F2, 0xDF49FF, 0x476DFF, 0x00B4F7, 0x00E0FF, 0x00E375, 0x03F42B, 0x78B82E, 0xE5E218, 0x787878, 0x000000, 0x000000, 0xFFFFFF, 0xFFF2BE, 0xF8B8B8, 0xF8B8D8, 0xFFB6FF, 0xFFC3FF, 0xC7D1FF, 0x9ADAFF, 0x88EDF8, 0x83FFDD, 0xB8F8B8, 0xF5F8AC, 0xFFFFB0, 0xF8D8F8, 0x000000, 0x000000)
        makeTables()
		setEmphasis(0)
	}
	
	def loadPALPalette(): Unit = {
		curTable = Array(0x525252, 0xB40000, 0xA00000, 0xB1003D, 0x740069, 0x00005B, 0x00005F, 0x001840, 0x002F10, 0x084A08, 0x006700, 0x124200, 0x6D2800, 0x000000, 0x000000, 0x000000, 0xC4D5E7, 0xFF4000, 0xDC0E22, 0xFF476B, 0xD7009F, 0x680AD7, 0x0019BC, 0x0054B1, 0x006A5B, 0x008C03, 0x00AB00, 0x2C8800, 0xA47200, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0xFFAB3C, 0xFF7981, 0xFF5BC5, 0xFF48F2, 0xDF49FF, 0x476DFF, 0x00B4F7, 0x00E0FF, 0x00E375, 0x03F42B, 0x78B82E, 0xE5E218, 0x787878, 0x000000, 0x000000, 0xFFFFFF, 0xFFF2BE, 0xF8B8B8, 0xF8B8D8, 0xFFB6FF, 0xFFC3FF, 0xC7D1FF, 0x9ADAFF, 0x88EDF8, 0x83FFDD, 0xB8F8B8, 0xF5F8AC, 0xFFFFB0, 0xF8D8F8, 0x000000, 0x000000)
        makeTables()
		setEmphasis(0);
	}
	
	def makeTables(): Unit = {
		// TODO
	}
	
	def setEmphasis(emph: Byte): Unit = {
		// TODO
	}
	
	def getEntry(yiq: Int): Byte = {
		// TODO
	}
	
	def getRed(rgb: Byte): Byte = {
		// TODO
	}
	
	def getGreen(rgb: Byte): Byte = {
		// TODO
	}
	
	def getBlue(rgb: Byte): Byte = {
		// TODO
	}
	
	def getRgb(r: Byte, g: Byte, b: Byte): Byte = {
		// TODO
	}
	
	def loadDefaultPalette(): Unit = {
		// TODO
	}
	
  }
  
  private class Tile {
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

  def startVBlank: Unit = {

  }

  def endScanline: Unit = {

  }

  def startFrame(): Unit = {

  }

  def setStatusFlag(flag: Int,value: Boolean): Unit = {

  }

}
