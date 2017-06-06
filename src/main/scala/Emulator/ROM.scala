package Emulator

import scala.language.dynamics
import scala.scalajs.js
import js.typedarray.ArrayBuffer
import js.{Dynamic, timers}
import js.typedarray.TypedArrayBuffer.wrap
import org.scalajs.jquery.{JQueryAjaxSettings, JQueryXHR, jQuery}
import org.scalajs.dom.ext.Ajax

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** class ROM
  *   this class represents the nes cartridge (Read Only Memory)
  *   it reads a file with an ajx request, performs some check on it
  *   and loads it into a byte array. It has functions that allow
  *   the program to access different part of this ROM and get its mapper
  *   It is based on the rom.js class from Ben Firsherman's javascript nes emulator
  */
class ROM(nes: NES) {

  //Mirroring types constants
  final val VerticalMirroring = 0
  final val HorizontalMirroring = 1
  final val FourscreenMirroring = 2
  final val SinglescreenMirroring = 3
  final val SinglescreenMirroring2 = 4
  final val SinglescreenMirroring3 = 5
  final val SinglescreenMirroring4 = 6
  final val ChrRomMirroring = 7

  //ROM parts
  var fullRom: Array[Byte] = _
  var trainer: Array[Byte] = _
  var prgRom: Array[Array[Byte]] = _
  var chrRom: Array[Array[Byte]] = _
  var vromTile: Array[Array[PPU.Tile]] = _

  //ROM sizes
  var prgRomSize: Int = 0
  var chrRomSize: Int = 0

  //mappers name
  val mapperName: Array[String] = Array.fill(92)("Unknown Mapper")

  //names of known and supported mapper
  mapperName( 0) = "Direct Access"
  mapperName( 1) = "Nintendo MMC1"
  mapperName( 2) = "UNROM"
  mapperName( 3) = "CNROM"
  mapperName( 4) = "Nintendo MMC3"
  mapperName( 5) = "Nintendo MMC5"
  mapperName( 6) = "FFE F4xxx"
  mapperName( 7) = "AOROM"
  mapperName( 8) = "FFE F3xxx"
  mapperName( 9) = "Nintendo MMC2"
  mapperName(10) = "Nintendo MMC4"
  mapperName(11) = "Color Dreams Chip"
  mapperName(12) = "FFE F6xxx"
  mapperName(15) = "100-in-1 switch"
  mapperName(16) = "Bandai chip"
  mapperName(17) = "FFE F8xxx"
  mapperName(18) = "Jaleco SS8806 chip"
  mapperName(19) = "Namcot 106 chip"
  mapperName(20) = "Famicom Disk System"
  mapperName(21) = "Konami VRC4a"
  mapperName(22) = "Konami VRC2a"
  mapperName(23) = "Konami VRC2a"
  mapperName(24) = "Konami VRC6"
  mapperName(25) = "Konami VRC4b"
  mapperName(32) = "Irem G-101 chip"
  mapperName(33) = "Taito TC0190/TC0350"
  mapperName(34) = "32kB ROM switch"

  mapperName(64) = "Tengen RAMBO-1 chip"
  mapperName(65) = "Irem H-3001 chip"
  mapperName(66) = "GNROM switch"
  mapperName(67) = "SunSoft3 chip"
  mapperName(68) = "SunSoft4 chip"
  mapperName(69) = "SunSoft5 FME-7 chip"
  mapperName(71) = "Camerica chip"
  mapperName(78) = "Irem 74HC161/32-based"
  mapperName(91) = "Pirate HK-SF3 chip"


  /** Returns the header of the rom in the form of a byte array */
  def getHeader: Array[Byte] = {
    fullRom.slice(0,16)
  }

  /** Returns the trainer, if has one, of the rom in the form of a byte array */
  def getTrainer: Array[Byte] = {
    if(hasTrainer) {
      fullRom.slice(16,528)
    } else {
      null
    }
  }

  /** Returns the prgRom of the rom in the form of an array of byte arrays */
  def getPrgRom: Array[Array[Byte]] = {
    prgRom = new Array(getPrgRomSize)
    var offset = 16 + (if(hasTrainer) 512 else 0)
    for (i <- 0 until getPrgRomSize) {
      prgRom(i) = new Array(0x4000)
      if(offset + 0x4000 <= fullRom.length) {
        prgRom(i) = fullRom.slice(offset, offset + 0x4000)
      }
      offset += 0x4000
    }
    prgRom
  }

  /** Returns the chrRom of the rom in the form of a byte array */
  def getChrRom: Array[Array[Byte]] = {
    chrRom = new Array(getChrRomSize)
    var offset = 16 + (if(hasTrainer) 512 else 0)
    offset += 0x4000*getPrgRomSize
    for (i <- 0 until getChrRomSize) {
      chrRom(i) = new Array(0x2000)
      if(offset + 0x2000 <= fullRom.length) {
        chrRom(i) = fullRom.slice(offset, offset + 0x2000)
      }
      offset += 0x2000
    }
    chrRom
  }

  /** Return and initialize vromtiles */
  def getVromTiles: Array[Array[PPU.Tile]] = {
    vromTile = new Array(getChrRomSize*2) // 2 tiles per 8k rom bank (1 per 4k bank)
    for (i <- 0 until (getChrRomSize*2)) {
      vromTile(i) = new Array[PPU.Tile](0x100)
      for (j <- 0 until 0x100) {
        vromTile(i)(j) = new PPU.Tile
      }
    }
    vromTile
  }

  /** Fill the tiles with chrrom content */
  def fillInTiles: Unit = {
    var tileIndex = 0
    var leftOver = 0
    for (v <- 0 until vromTile.length) { //2x chrrom total size
      for (i <- 0 until 0x1000) { // 1/2 chrrom element (bank)
        tileIndex = i >> 4
        leftOver = i % 0x10
        if ((v % 2) == 0) { //Lower tile
          if (leftOver < 8) {
            vromTile(v)(tileIndex).setScanline(leftOver, chrRom(v / 2)(i), chrRom(v / 2)(i + 8))
          } else {
            vromTile(v)(tileIndex).setScanline(leftOver - 8, chrRom(v / 2)(i - 8), chrRom(v / 2)(i))
          }
        } else { //Upper tile
          if (leftOver < 8) {
            vromTile(v)(tileIndex).setScanline(leftOver, chrRom(v / 2)(i + 0x1000), chrRom(v / 2)(i + 8 + 0x1000))
          } else {
            vromTile(v)(tileIndex).setScanline(leftOver - 8, chrRom(v / 2)((i - 8) + 0x1000), chrRom(v / 2)(i + 0x1000))
          }
        }
      }
    }
  }

  /** Returns true when the rom has battery ram */
  def hasBatteryRam: Boolean = {
    (getHeader(6) & 2) != 0
  }

  /** Returns true when the rom has the 512 bytes of trainer before the prgrom */
  def hasTrainer: Boolean = {
    (getHeader(6) & 4) != 0
  }

  /** Returns the size in 16KB of the prgrom */
  def getPrgRomSize: Int = {
    prgRomSize
  }

  /** Returns the size in 8KB of the chrrom */
  def getChrRomSize: Int = {
    chrRomSize
  }

  /** Store size of both chr and prg rom into vriables */
  def setSizes: Unit = {
    // Scala Bytes are signed, but we want it unsigned, so we ll be using the modulo operator
    prgRomSize = (getHeader(4).toInt + 0x100) % 0x100 // conversion from signed to unsigned
    chrRomSize = (getHeader(5).toInt + 0x100) % 0x100 // conversion from signed to unsigned
  }

  /** Returns the mirroring type as an `Int`.
    *  - 0: vertical mirroring
    *  - 1: horizontal mirroring
    *  - 2: fourscreen mirroring
    */
  def getMirroringType: Int = {
    val flagByte = getHeader(6)
    if((flagByte & 8) != 0){
      FourscreenMirroring
    } else if((flagByte & 1) != 0) {
      VerticalMirroring
    } else {
      HorizontalMirroring
    }
  }

  /** Returns the mapper type in the form of an integer */
  def getMapperNum: Int = {
    (getHeader(6) >> 4) | (getHeader(7) & 240) //0xF0
  }

  /** Returns true if the mapper is supported */
  def isMapperSupported(num: Int): Boolean = {
    num >= 0 && num < mapperName.length && mapperName(num) != "Unknown Mapper"
  }

  /** Performs a check on the mapper number validity and return a newly created mapper of the mapper class */
  def createMapper: Mapper = {
    val num = getMapperNum
    if(isMapperSupported(num)) {
      nes.mapperTable(num)()
    } else {
      Dynamic.global.console.log(s"Unsupported mapper, $num")
      null
    }
  }

  /** Returns the name of the mapper */
  def getMapperName: String = {
    val num = getMapperNum
    if(isMapperSupported(num)) {
      mapperName(num)
    } else {
      "Unknown Mapper"
    }
  }

  /** Loads the requested rom into an array of byte, while performing some checks on the header */
  def openRom(url: String): Future[Any] = {

    //ajax request to get the ROM in a byte array format
    val ajaxReq = Ajax.get(url, null, 0, Map.empty, false, "arraybuffer")
    val ret: Future[Any] = ajaxReq.map {
      case xhr =>
        val byteBuffer = wrap( xhr.response.asInstanceOf[ArrayBuffer] )
        fullRom = new Array(byteBuffer.capacity())
        byteBuffer.get(fullRom)
        if(checkRom) {//performs a check before validating
          Dynamic.global.console.log(s"Successfully loaded $url")
          setSizes
          trainer = getTrainer
          prgRom = getPrgRom
          chrRom = getChrRom
          vromTile = getVromTiles
          fillInTiles
          nes.mmap = createMapper
        } else {
          Dynamic.global.console.log("File is not a valid ROM")
          fullRom = null
        }
    }
    ajaxReq.onFailure {
      case e =>
        Dynamic.global.console.log(s"Failed to load ROM at $url")
        e.printStackTrace()
    }
    ret

  }

  /** Checks if the fullROM meets the .nes header requirement */
  def checkRom: Boolean = {
    if(fullRom != null) {
      val header = getHeader
      header(0) == 'N' &&
        header(1) == 'E' &&
        header(2) == 'S' &&
        header(10) == 0 &&
        header(11) == 0 &&
        header(12) == 0 &&
        header(13) == 0 &&
        header(14) == 0 &&
        header(15) == 0
    } else {
      false
    }
  }



}
