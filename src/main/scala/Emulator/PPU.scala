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
        var basey : Int = (scala.math.floor(index / 8) * 4).asInstanceOf[Int]
        var add : Byte = 0
        var tx, ty : Int = 0
		var attindex : Int = 0
		
		var sqy, sqx : Int = 0
		for (sqy <- 1 to 2; sqx <- 1 to 2) {
			add = (((value>>(2*(sqy*2+sqx))).asInstanceOf[Byte])&3).asInstanceOf[Byte] // Bit operators
			for (y <- 1 to 2; x <- 1 to 2) {
				tx = basex+sqx*2+x
                ty = basey+sqy*2+y
                attindex = ty*width+tx
                attrib(ty*width+tx) = ((add<<2)&12).asInstanceOf[Byte]
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
		if (emph != currentEmph) {
			currentEmph = emph
			var i : Int = 0
			for (i <- 1 to 64) curTable(i) = emphTable(emph)(i)
		}
	}
	
	def getEntry(yiq: Int): Byte = {
		return curTable(yiq)
	}
	
	def getRed(rgb: Byte): Byte = {
		return ((rgb>>16)&0xFF).asInstanceOf[Byte]
	}
	
	def getGreen(rgb: Byte): Byte = {
		return ((rgb>>8)&0xFF).asInstanceOf[Byte]
	}
	
	def getBlue(rgb: Byte): Byte = {
		return (rgb&0xFF).asInstanceOf[Byte]
	}
	
	def getRgb(r: Byte, g: Byte, b: Byte): Byte = {
		return ((r<<16)|(g<<8)|(b))
	}
	
	def loadDefaultPalette(): Unit = {
		curTable(0) = getRgb(117,117,117);
        curTable(1) = getRgb(39, 27,143);
        curTable(2) = getRgb(0,0,171);
        curTable(3) = getRgb(71,0,159);
        curTable(4) = getRgb(143,0,119);
        curTable(5) = getRgb(171,0, 19);
        curTable(6) = getRgb(167,0,0);
        curTable(7) = getRgb(127,11,0);
        curTable(8) = getRgb(67,47,0);
        curTable(9) = getRgb(0,71,0);
        curTable(10) = getRgb(0,81,0);
        curTable(11) = getRgb(0,63,23);
        curTable(12) = getRgb(27,63,95);
        curTable(13) = getRgb(0,0,0);
        curTable(14) = getRgb(0,0,0);
        curTable(15) = getRgb(0,0,0);
        curTable(16) = getRgb(188,188,188);
        curTable(17) = getRgb(0,115,239);
        curTable(18) = getRgb(35,59,239);
        curTable(19) = getRgb(131,0,243);
        curTable(20) = getRgb(191,0,191);
        curTable(21) = getRgb(231,0,91);
        curTable(22) = getRgb(219,43,0);
        curTable(23) = getRgb(203,79,15);
        curTable(24) = getRgb(139,115,0);
        curTable(25) = getRgb(0,151,0);
        curTable(26) = getRgb(0,171,0);
        curTable(27) = getRgb(0,147, 59);
        curTable(28) = getRgb(0,131,139);
        curTable(29) = getRgb(0,0,0);
        curTable(30) = getRgb(0,0,0);
        curTable(31) = getRgb(0,0,0);
        curTable(32) = getRgb(255,255,255);
        curTable(33) = getRgb(63,191,255);
        curTable(34) = getRgb(95,151,255);
        curTable(35) = getRgb(167,139,253);
        curTable(36) = getRgb(247,123,255);
        curTable(37) = getRgb(255,119,183);
        curTable(38) = getRgb(255,119,99);
        curTable(39) = getRgb(255,155,59);
        curTable(40) = getRgb(243,191,63);
        curTable(41) = getRgb(131,211,19);
        curTable(42) = getRgb(79,223,75);
        curTable(43) = getRgb(88,248,152);
        curTable(44) = getRgb(0,235,219);
        curTable(45) = getRgb(0,0,0);
        curTable(46) = getRgb(0,0,0);
        curTable(47) = getRgb(0,0,0);
        curTable(48) = getRgb(255,255,255);
        curTable(49) = getRgb(171,231,255);
        curTable(50) = getRgb(199,215,255);
        curTable(51) = getRgb(215,203,255);
        curTable(52) = getRgb(255,199,255);
        curTable(53) = getRgb(255,199,219);
        curTable(54) = getRgb(255,191,179);
        curTable(55) = getRgb(255,219,171);
        curTable(56) = getRgb(255,231,163);
        curTable(57) = getRgb(227,255,163);
        curTable(58) = getRgb(171,243,191);
        curTable(59) = getRgb(179,255,207);
        curTable(60) = getRgb(159,255,243);
        curTable(61) = getRgb(0,0,0);
        curTable(62) = getRgb(0,0,0);
        curTable(63) = getRgb(0,0,0);
        
        makeTables();
		setEmphasis(0);
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
