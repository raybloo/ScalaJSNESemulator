package Emulator
import scala.annotation.switch
import scala.scalajs.js.Dynamic

/** Picture Processing Unit class, permits to generate the images/video for the emulator.
  * Uses registers, a memory map, palettes, sprites and several tables.
  */
class PPU(nes: NES) {
  import PPU._

  var vramMem : Array[Int] = null
  var spriteMem : Array[Int] = null
  var vramAddress : Int = _
  var vramTmpAddress : Int = _
  var vramBufferedReadValue : Int = 0
  var firstWrite : Boolean = true
  var sramAddress : Int = 0
  var currentMirroring : Int = -1
  var requestEndFrame : Boolean = false
  var nmiOk : Boolean = false
  var dummyCycleToggle : Boolean = false
  var nmiCounter: Int = 0
  var scanlineAlreadyRendered : Boolean = false
  var f_nmiOnVblank : Int = 0
  var f_spriteSize : Int = 0
  var f_bgPatternTable : Int = 0
  var f_spPatternTable : Int = 0
  var f_addrInc : Int = 0
  var f_nTblAddress : Int = 0
  var f_color : Int = 0
  var f_spVisibility : Int = 0
  var f_bgVisibility : Int = 0
  var f_spClipping : Int = 0
  var f_bgClipping : Int = 0
  var f_dispType : Int = 0
  var cntFV : Int = 0
  var cntV : Int = 0
  var cntH : Int = 0
  var cntVT : Int = 0
  var cntHT : Int = 0
  var regFV : Int = 0
  var regV : Int = 0
  var regH : Int = 0
  var regVT : Int = 0
  var regHT : Int = 0
  var regFH : Int = 0
  var regS : Int = 0
  var curNt : Int = -1
  var attrib: Array[Int] = null
  var buffer : Array[Int] = null
  var prevBuffer : Array[Int] = null
  var bgbuffer : Option[Array[Int]] = None
  var pixrendered : Array[Int] = null

  var validTileData : Boolean = false
  var scantile : Array[Tile] = null
  var scanline: Int = 0
  var lastRenderedScanline : Int = -1
  var curX : Int = 0
  var sprX : Array[Int] = null
  var sprY : Array[Int] = null
  var sprTile : Array[Int] = null
  var sprCol : Array[Int] = null
  var vertFlip : Array[Boolean] = null
  var horiFlip : Array[Boolean] = null
  var bgPriority : Array[Boolean] = null
  var spr0HitX : Int = 0
  var spr0HitY : Int = 0
  var hitSpr0 : Boolean = false
  var sprPalette : Array[Int] = null
  var imgPalette : Array[Int] = null
  var ptTile : Array[Tile] = _
  var ntable1 : Array[Int] = null
  var nameTable : Array[NameTable] = _
  var vramMirrorTable : Array[Int] = null
  var palTable : PaletteTable = _

  // Rendering Options:
  var showSpr0Hit: Boolean = false
  var clipToTvSize: Boolean = true

  // Status flags:
  val StatusVRAMWrite: Byte =  4
  val StatusSLSpriteCount: Byte =  5
  val StatusSprite0Hit: Byte =  6
  val StatusVBlank: Byte =  7

  /** Initializing/reseting all variables at reset. */
  def reset(): Unit = {
    // Memory
    vramMem = Array.fill[Int](0x8000)(0)
    spriteMem = Array.fill[Int](0x100)(0)

    // VRAM I/O:
    vramAddress = -1
    vramTmpAddress = -1
    vramBufferedReadValue = 0
    firstWrite = true         // VRAM/Scroll Hi/Lo latch

    // SPR-RAM I/O:
    sramAddress = 0 // 8-bit only.

    currentMirroring = -1
    requestEndFrame = false
    nmiOk = false
    dummyCycleToggle = false
    validTileData = false
    nmiCounter = 0
    scanlineAlreadyRendered = false

    // Control Flags Register 1:
    f_nmiOnVblank = 0         // NMI on VBlank. 0=disable, 1=enable
    f_spriteSize = 0          // Sprite size. 0=8x8, 1=8x16
    f_bgPatternTable = 0      // Background Pattern Table address. 0=0x0000,1=0x1000
    f_spPatternTable = 0      // Sprite Pattern Table address. 0=0x0000,1=0x1000
    f_addrInc = 0             // PPU Address Increment. 0=1,1=32
    f_nTblAddress = 0         // Name Table Address. 0=0x2000,1=0x2400,2=0x2800,3=0x2C00

    // Control Flags Register 2:
    f_color = 0          // Background color. 0=black, 1=blue, 2=green, 4=red
    f_spVisibility = 0   // Sprite visibility. 0=not displayed,1=displayed
    f_bgVisibility = 0   // Background visibility. 0=Not Displayed,1=displayed
    f_spClipping = 0     // Sprite clipping. 0=Sprites invisible in left 8-pixel column,1=No clipping
    f_bgClipping = 0     // Background clipping. 0=BG invisible in left 8-pixel column, 1=No clipping
    f_dispType = 0       // Display type. 0=color, 1=monochrome

    // Counters:
    cntFV = 0
    cntV = 0
    cntH = 0
    cntVT = 0
    cntHT = 0

    // Registers:
    regFV = 0
    regV = 0
    regH = 0
    regVT = 0
    regHT = 0
    regFH = 0
    regS = 0

    // These are temporary variables used in rendering and sound procedures.
    // Their states outside of those procedures can be ignored.
    curNt = -1

    // Variables used when rendering:
    attrib = new Array(32)
    buffer = new Array(256*240)
    prevBuffer = new Array(256*240)
    bgbuffer = Some(new Array(256*240))
    pixrendered = new Array(256*240)

    validTileData = false

    scantile = new Array(32)

    // Initialize misc vars:
    scanline = 0
    lastRenderedScanline = -1
    curX = 0

    // Sprite data:
    sprX = new Array(64)        // X coordinate
    sprY = new Array(64)        // Y coordinate
    sprTile = new Array(64)     // Tile Index (into pattern table)
    sprCol = new Array(64)      // Upper two bits of color
    vertFlip = new Array(64)    // Vertical Flip
    horiFlip = new Array(64)    // Horizontal Flip
    bgPriority = new Array(64)  // Background priority
    spr0HitX = 0                // Sprite #0 hit X coordinate
    spr0HitY = 0                // Sprite #0 hit Y coordinate
    hitSpr0 = false

    // Palette data:
    sprPalette = new Array(16)
    imgPalette = new Array(16)

    // Create pattern table tile buffers:
    ptTile = new Array(512)
    for (i <- 0 until 512) ptTile(i) = new Tile()

    // Create nametable buffers:
    // Name table data:
    ntable1 = new Array(4)
    nameTable = new Array(4)

    for (i <- 0 until 4) nameTable(i) = new NameTable(32, 32, "Nt"+i)

    // Initialize mirroring lookup table:
    vramMirrorTable = new Array(0x8000)
    for (i <- 1 until 0x8000) vramMirrorTable(i) = i

    palTable = new PaletteTable()
    palTable.loadNTSCPalette()

    updateControlReg1(0)
    updateControlReg2(0)
  }

  /** Sets Nametable mirroring with the given mirroring value. */
  def setMirroring(mirroring: Int): Unit = {
    if (mirroring == currentMirroring) return

    currentMirroring = mirroring
    triggerRendering()

    // Remove mirroring:
    if (vramMirrorTable == null) vramMirrorTable = new Array(0x8000)
    for (i <- 0 until 0x8000) vramMirrorTable(i) = i

    // Palette mirroring:
    defineMirrorRegion(0x3f20,0x3f00,0x20)
    defineMirrorRegion(0x3f40,0x3f00,0x20)
    defineMirrorRegion(0x3f80,0x3f00,0x20)
    defineMirrorRegion(0x3fc0,0x3f00,0x20)

    // Additional mirroring:
    defineMirrorRegion(0x3000,0x2000,0xf00)
    defineMirrorRegion(0x4000,0x0000,0x4000)

    if (mirroring == nes.rom.HorizontalMirroring) {
      // Horizontal mirroring.
      ntable1(0) = 0
      ntable1(1) = 0
      ntable1(2) = 1
      ntable1(3) = 1

      defineMirrorRegion(0x2400,0x2000,0x400)
      defineMirrorRegion(0x2c00,0x2800,0x400)
    } else if (mirroring == nes.rom.VerticalMirroring) {
      // Vertical mirroring.
      ntable1(0) = 0
      ntable1(1) = 1
      ntable1(2) = 0
      ntable1(3) = 1

      defineMirrorRegion(0x2800,0x2000,0x400)
      defineMirrorRegion(0x2c00,0x2400,0x400)
    } else if (mirroring == nes.rom.SinglescreenMirroring) {
      // Single Screen mirroring
      ntable1(0) = 0
      ntable1(1) = 0
      ntable1(2) = 0
      ntable1(3) = 0

      defineMirrorRegion(0x2400,0x2000,0x400)
      defineMirrorRegion(0x2800,0x2000,0x400)
      defineMirrorRegion(0x2c00,0x2000,0x400)
    } else if (mirroring == nes.rom.SinglescreenMirroring2) {
      ntable1(0) = 1
      ntable1(1) = 1
      ntable1(2) = 1
      ntable1(3) = 1

      defineMirrorRegion(0x2400,0x2400,0x400)
      defineMirrorRegion(0x2800,0x2400,0x400)
      defineMirrorRegion(0x2c00,0x2400,0x400)
    } else {
      // Assume Four-screen mirroring.
      ntable1(0) = 0
      ntable1(1) = 1
      ntable1(2) = 2
      ntable1(3) = 3
    }
  }

  /**
    * Definea a mirrored area in the address lookup table, while assuming the regions don't overlap.
    * fromStart :
    *      where to start the mirroring change.
    *      where to start the mirroring change.
    * toStart :
    *      region that is physically in memory.
    * size :
    *      size of change
    */
  def defineMirrorRegion(fromStart: Int, toStart: Int, size: Int): Unit = for (i <- 0 until size) vramMirrorTable(fromStart+i) = toStart+i

  /** Interrupt the CPU and render everything. */
  def startVBlank(): Unit = {
    // Do non-maskable interrupt:
    nes.cpu.requestIrq(1)

    // Make sure everything is rendered:
    if (lastRenderedScanline < 239) renderFramePartially(lastRenderedScanline+1, 240-lastRenderedScanline)

    // End frame:
    endFrame()

    // Reset scanline counter:
    lastRenderedScanline = -1
  }

  /** Render the scanline when necessary and check sprite 0 hit when necessary.  */
  def endScanline(): Unit = {
    (scanline: @switch) match {
      case 19 =>
        // Dummy scanline, may be variable length:
        if (dummyCycleToggle) {
          // Remove dead cycle at end of scanline for next scanline:
          curX = 1
          dummyCycleToggle = !dummyCycleToggle
        }

      case 20 =>
        // Clear VBlank flag:
        setStatusFlag(StatusVBlank,false)

        // Clear Sprite #0 hit flag:
        setStatusFlag(StatusSprite0Hit,false)
        hitSpr0 = false
        spr0HitX = -1
        spr0HitY = -1

        if (f_bgVisibility == 1 || f_spVisibility == 1) {
          // Update counters:
          cntFV = regFV
          cntV = regV
          cntH = regH
          cntVT = regVT
          cntHT = regHT

          // Render dummy scanline:
          if (f_bgVisibility==1) renderBgScanline(false,0)

        }

        // Check sprite 0 hit for first scanline:
        if (f_bgVisibility == 1 && f_spVisibility == 1) checkSprite0(0)

        // Clock mapper IRQ Counter:
        if (f_bgVisibility == 1 || f_spVisibility == 1) nes.mmap.clockIrqCounter

      case 261 => // Dead scanline, no rendering.
        // Set VINT:
        setStatusFlag(StatusVBlank, true)
        requestEndFrame = true
        nmiCounter = 9

        // Wrap around:
        scanline = -1 // will be incremented to 0

      case default =>
        if (scanline >= 21 && scanline <= 260) {

          // Render normally:
          if (f_bgVisibility == 1) {
            if (!scanlineAlreadyRendered) {
              // update scroll:
              cntHT = regHT
              cntH = regH
              renderBgScanline(true, scanline+1-21)
            }

            scanlineAlreadyRendered = false

            // Check for sprite 0 (next scanline):
            if (!hitSpr0 && f_spVisibility == 1) {
              if (sprX(0) >= -7 && sprX(0) < 256 && (sprY(0) + 1) <= (scanline - 20) && (sprY(0) + 1 + (if (f_spriteSize == 0) 8 else 16)) >= (scanline - 20)) {
                if (checkSprite0(scanline - 20)) {
                  hitSpr0 = true
                }
              }
            }
          }

          // Clock mapper IRQ Counter:
          if (f_bgVisibility == 1 || f_spVisibility == 1) nes.mmap.clockIrqCounter
        }
    }

    scanline += 1
    regsToAddress()
    cntsToAddress()
  }

  /** Create frame background color and send it to buffer. */
  def startFrame(): Unit = {
    // Set background color:
    var bgColor : Int = 0

    // Color display. f_color determines color emphasis. Use first entry of image palette as BG color.
    if (f_dispType == 0) {
        bgColor = imgPalette(0)
    } else { // Monochrome display.f_color determines the bg color.
      (f_color: @switch) match {
          case 0 =>
            // Black
            bgColor = 0x00000
          case 1 =>
            // Green
            bgColor = 0x00FF00
          case 2 =>
            // Blue
            bgColor = 0xFF0000
          case 3 =>
            // Invalid. Use black.
            bgColor = 0x00000
          case 4 =>
            // Red
            bgColor = 0x0000FF
          case default =>
            // Invalid. Use black.
            bgColor = 0x00000
      }
    }
    for (i <- 0 until 256*240) buffer(i) = bgColor
    for (i <- 0 until pixrendered.length) pixrendered(i) = 65
  }

  /** Finalize the frame by putting sprites, than show it on UI. */
  def endFrame(): Unit = {
    // Draw spr#0 hit coordinates:
    if (showSpr0Hit) {
      // Spr 0 position:
      if (sprX(0) >= 0 && sprX(0) < 256 && sprY(0) >= 0 && sprY(0) < 240) {
        for (i <- 0 until 256) buffer((sprY(0)<<8)+i) = 0xFF5555
        for (i <- 0 until 240) buffer((i<<8)+sprX(0)) = 0xFF5555
      }

      // Hit position:
      if (spr0HitX >= 0 && spr0HitX < 256 && spr0HitY >= 0 && spr0HitY < 240) {
        for (i <- 0 until 256) buffer((spr0HitY<<8)+i) = 0x55FF55

        for (i <- 0 until 240) buffer((i<<8)+spr0HitX) = 0x55FF55
      }
    }

    // if either the sprites or the background should be clipped, both are clipped after rendering is finished.
    if (clipToTvSize || f_bgClipping == 0 || f_spClipping == 0) {
      // Clip left 8-pixels column:
      for (y <- 0 until 240; x <- 0 until 8) buffer((y<<8)+x) = 0
    }

    // Clip right 8-pixels column too:
    if (clipToTvSize) for (y <- 0 until 240; x <- 0 until 8) buffer((y<<8)+255-x) = 0

    // Clip top and bottom 8 pixels:
    if (clipToTvSize) {
      for (y <- 0 until 8; x <- 0 until 256) {
        buffer((y<<8)+x) = 0
        buffer(((239-y)<<8)+x) = 0
      }
    }

    if (nes.showDisplay) {
      nes.ui.writeFrame(buffer, prevBuffer)
    }
  }

  /** Render, then update wanted values */
  def updateControlReg1(value: Int): Unit = {
    triggerRendering()

    f_nmiOnVblank = (value>>7)&1
    f_spriteSize = (value>>5)&1
    f_bgPatternTable =(value>>4)&1
    f_spPatternTable =(value>>3)&1
    f_addrInc = (value>>2)&1
    f_nTblAddress = value&3

    regV = (value>>1)&1
    regH = value&1
    regS = (value>>4)&1
  }

  /** Render, then update wanted values */
  def updateControlReg2(value: Int): Unit = {
    triggerRendering()

    f_color = (value>>5)&7
    f_spVisibility = (value>>4)&1
    f_bgVisibility = (value>>3)&1
    f_spClipping = (value>>2)&1
    f_bgClipping = (value>>1)&1
    f_dispType = value&1

    if (f_dispType == 0) palTable.setEmphasis(f_color)
    updatePalettes()
  }

  def setStatusFlag(flag: Int, value: Boolean): Unit = {
    var n = 1<<flag
    nes.cpu.memory(0x2002) = ((nes.cpu.unsign(nes.cpu.memory(0x2002)) & (255-n)) | (if (value) n else 0)).toByte
  }

  /** CPU Register $2002: Read the Status Register. */
  def readStatusRegister(): Byte = {
    var tmp : Byte = nes.cpu.memory(0x2002)

    // Reset scroll & VRAM Address toggle:
    firstWrite = true

    // Clear VBlank flag:
    setStatusFlag(StatusVBlank, false)

    // Fetch status data:
    return tmp
  }

  /** CPU Register $2003: Write the SPR-RAM address that is used for sramWrite (Register 0x2004 in CPU memory map) */
  def writeSRAMAddress(address: Int): Unit = sramAddress = address

  /** CPU Register $2004 (R): Read from SPR-RAM (Sprite RAM). The address should be set first. */
  def sramLoad(): Int = spriteMem(sramAddress)

  /** CPU Register $2004 (R): Write to SPR-RAM (Sprite RAM). The address should be set first. */
  def sramWrite(value: Int): Unit = {
    spriteMem(sramAddress) = value
    spriteRamWriteUpdate(sramAddress, value)
    sramAddress += 1 // Increment address
    sramAddress %= 0x100
  }

  /** CPU Register $2005: Write to scroll registers. The first write is the vertical offset, the second is the horizontal offset. */
  def scrollWrite(value: Int): Unit = {
    triggerRendering()

    if (firstWrite) {
      // First write, horizontal scroll:
      regHT = (value>>3)&31
      regFH = value&7
    } else {
      // Second write, vertical scroll:
      regFV = value&7
      regVT = (value>>3)&31
    }

    firstWrite = !firstWrite
  }

  /** CPU Register $2006: Sets the address used when reading/writing from/to VRAM. The first write sets the high byte, the second the low byte. */
  def writeVRAMAddress(address: Int): Unit = {
    if (firstWrite) {
      regFV = (address>>4)&3
      regV = (address>>3)&1
      regH = (address>>2)&1
      regVT = (regVT&7) | ((address&3)<<3)
    } else {
      triggerRendering()

      regVT = (regVT&24) | ((address>>5)&7)
      regHT = address&31

      cntFV = regFV
      cntV = regV
      cntH = regH
      cntVT = regVT
      cntHT = regHT

      checkSprite0(scanline-20)
    }

    firstWrite = !firstWrite

    // Invoke mapper latch:
    cntsToAddress()
    if (vramAddress < 0x2000) nes.mmap.latchAccess(vramAddress)
  }

  /** CPU Register $2007(R): Read from PPU memory. The address should be set first. */
  def vramLoad(): Int = {
    var tmp : Int = -1

    cntsToAddress()
    regsToAddress()

    // If address is in range 0x0000-0x3EFF, return buffered values:
    if (vramAddress <= 0x3EFF) {
      tmp = vramBufferedReadValue

      // Update buffered value:
      if (vramAddress < 0x2000) {
        vramBufferedReadValue = vramMem(vramAddress)
      } else {
        vramBufferedReadValue = mirroredLoad(vramAddress)
      }

      // Mapper latch access:
      if (vramAddress < 0x2000) nes.mmap.latchAccess(vramAddress)

      // Increment by either 1 or 32, depending on d2 of Control Register 1:
      vramAddress += (if (f_addrInc == 1) 32 else 1)

      cntsFromAddress()
      regsFromAddress()

      return tmp // Return the previous buffered value.
    }

    // No buffering in this mem range. Read normally.
    tmp = mirroredLoad(vramAddress)

    // Increment by either 1 or 32, depending on d2 of Control Register 1:
    vramAddress += (if (f_addrInc == 1) 32 else 1)

    cntsFromAddress()
    regsFromAddress()

    return tmp
  }

  /** CPU Register $2007(W): Write to PPU memory. The address should be set first. */
  def vramWrite(value: Int): Unit = {
    triggerRendering()
    cntsToAddress()
    regsToAddress()

    if (vramAddress >= 0x2000) {
      // Mirroring is used.
      mirroredWrite(vramAddress, value)
    } else {
      // Write normally.
      writeMem(vramAddress, value)

      // Invoke mapper latch:
      nes.mmap.latchAccess(vramAddress)
    }

    // Increment by either 1 or 32, depending on d2 of Control Register 1:
    vramAddress += (if (f_addrInc == 1) 32 else 1)
    regsFromAddress()
    cntsFromAddress()
  }

  /**  CPU Register $4014: Write 256 bytes of main memory into Sprite RAM. */
  def sramDMA(value: Int): Unit = {
    var baseAddress : Int = value * 0x100
    var data : Int = 0

    for (i <- sramAddress to 256) {
      data = nes.cpu.memory(baseAddress+i)
      spriteMem(i) = data
      spriteRamWriteUpdate(i, data)
    }

    nes.cpu.haltCycles(513)
  }

  /** Updates the scroll registers from a new VRAM address. */
  def regsFromAddress(): Unit = {
    var address : Int = (vramTmpAddress>>8)&0xFF

    regFV = (address>>4)&7
    regV = (address>>3)&1
    regH = (address>>2)&1
    regVT = (regVT&7) | ((address&3)<<3)

    address = vramTmpAddress&0xFF
    regVT = (regVT&24) | ((address>>5)&7)
    regHT = address&31
  }

  /** Updates the scroll registers from a new VRAM address. */
  def cntsFromAddress(): Unit = {
    var address : Int = (vramAddress>>8)&0xFF

    cntFV = (address>>4)&3
    cntV = (address>>3)&1
    cntH = (address>>2)&1
    cntVT = (cntVT&7) | ((address&3)<<3)

    address = vramAddress&0xFF
    cntVT = (cntVT&24) | ((address>>5)&7)
    cntHT = address&31
  }

  /* Updates the VRAM address from the scroll registers. */
  def regsToAddress(): Unit = {
    var b1 : Int = (regFV&7)<<4
    b1 |= (regV&1)<<3
    b1 |= (regH&1)<<2
    b1 |= (regVT>>3)&3

    var b2 : Int = (regVT&7)<<5
    b2 |= regHT&31

    vramTmpAddress = ((b1<<8) | b2)&0x7FFF
  }

  /* Updates the VRAM address from the scroll registers. */
  def cntsToAddress(): Unit = {
    var b1 : Int = (cntFV&7)<<4
    b1 |= (cntV&1)<<3
    b1 |= (cntH&1)<<2
    b1 |= (cntVT>>3)&3

    var b2 : Int = (cntVT&7)<<5
    b2 |= cntHT&31

    vramAddress = ((b1<<8) | b2)&0x7FFF
  }

  /** Updates the scroll registers with tile count */
  def incTileCounter(count: Int): Unit = {
    for (i <- count to 1 by -1) {
      cntHT+= 1
      if (cntHT == 32) {
        cntHT = 0
        cntVT += 1
        if (cntVT >= 30) {
          cntH+= 1
          if(cntH == 2) {
            cntH = 0
            cntV += 1
            if (cntV == 2) {
              cntV = 0
              cntFV += 1
              cntFV &= 0x7
            }
          }
        }
      }
    }
  }

  /** Reads from memory, taking into account mirroring/mapping of address ranges. */
  def mirroredLoad(address: Int): Int = vramMem(vramMirrorTable(address))

  /** Writes to memory, taking into account mirroring/mapping of address ranges. */
  def mirroredWrite(address: Int, value: Int): Unit = {
    if (address >= 0x3f00 && address < 0x3f20) {
      // Palette write mirroring.
      if (address == 0x3F00 || address == 0x3F10) {
        writeMem(0x3F00, value)
        writeMem(0x3F10, value)

      } else if (address == 0x3F04 || address == 0x3F14) {
        writeMem(0x3F04, value)
        writeMem(0x3F14, value)
      } else if (address == 0x3F08 || address == 0x3F18) {
        writeMem(0x3F08,value)
        writeMem(0x3F18,value)
      } else if (address == 0x3F0C || address == 0x3F1C) {
        writeMem(0x3F0C,value)
        writeMem(0x3F1C,value)
      } else writeMem(address, value)
    } else {
      // Use lookup table for mirrored address:
      if (address < vramMirrorTable.length) {
        writeMem(vramMirrorTable(address), value)
      } else Dynamic.global.alert("Invalid VRAM address: " + address.toString())
    }
  }

  /** Start rendering frames at scanline. */
  def triggerRendering(): Unit = {
    if (scanline >= 21 && scanline <= 260) {
      // Render sprites, and combine:
      renderFramePartially( lastRenderedScanline + 1, scanline - 21 - lastRenderedScanline)

      // Set last rendered scanline:
      lastRenderedScanline = scanline - 21
    }
  }

  /** Start rendering sprites and background at given scanline */
  def renderFramePartially(startScan: Int, scanCount: Int): Unit = {
    if (f_spVisibility == 1) renderSpritesPartially(startScan, scanCount, true)

    if(f_bgVisibility == 1) {
      var si : Int = startScan<<8
      var ei : Int = (startScan+scanCount)<<8
      if (ei > 0xF000) ei = 0xF000

      for (destIndex <- si until ei) if (pixrendered(destIndex) > 0xFF) buffer(destIndex) = bgbuffer.getOrElse(Array.fill(buffer.size)(0))(destIndex)
    }

    if (f_spVisibility == 1) renderSpritesPartially(startScan, scanCount, false)

    validTileData = false
  }

  /** Renders the tile data for a given scanline */
  def renderBgScanline(pbgbuffer: Boolean, scan: Int): Unit = {
    var baseTile : Int = (if (regS == 0) 0 else 256)
    var destIndex : Int = (scan<<8) - regFH

    curNt = ntable1(cntV + cntV + cntH)
    cntHT = regHT
    cntH = regH
    curNt = ntable1(cntV + cntV + cntH)

    if (scan < 240 && (scan - cntFV) >= 0) {
      val tscanoffset : Int = cntFV<<3
      val targetBuffer : Array[Int] = bgbuffer.getOrElse(buffer)

      var t : Tile = null
      var tpix : Array[Int] = null
      var att, col : Int = 0

      for (tile <- 0 until 32) {
        if (!((validTileData && scantile(tile) == null)||(!validTileData && ptTile(baseTile + nameTable(curNt).getTileIndex(cntHT, cntVT)) == null))) {
          if (scan >= 0) {
            // Fetch tile & attrib data:
            if (validTileData) {
              // Get data from array:
              t = scantile(tile)
              tpix = t.pix
              att = attrib(tile)
            } else {
              // Fetch data:
              t = ptTile(baseTile + nameTable(curNt).getTileIndex(cntHT, cntVT))
              tpix = t.pix
              att = nameTable(curNt).getAttrib(cntHT, cntVT)
              scantile(tile) = t
              attrib(tile) = att
            }

            // Render tile scanline:
            var sx: Int = 0
            var x: Int = (tile << 3) - regFH

            if (x > -8) {
              if (x < 0) {
                destIndex -= x
                sx = -x
              }

              if (t.opaque(cntFV)) {
                while (sx < 8) {
                  targetBuffer(destIndex) = imgPalette(tpix(tscanoffset + sx) + att)
                  pixrendered(destIndex) |= 256
                  destIndex += 1
                  sx += 1
                }
              } else {
                while (sx < 8) {
                  col = tpix(tscanoffset + sx)
                  if (col != 0) {
                    targetBuffer(destIndex) = imgPalette(col + att)
                    pixrendered(destIndex) |= 256
                  }
                  destIndex += 1
                  sx += 1
                }
              }
            }
          }

          // Increase Horizontal Tile Counter:
          cntHT += 1
          if (cntHT == 32) {
            cntHT = 0
            cntH += 1
            cntH %= 2
            curNt = ntable1((cntV << 1) + cntH)
          }
        }

        // Tile data for one row should now have been fetched, so the data in the array is valid.
        validTileData = true
      }
    }

    // update vertical scroll:
    cntFV += 1
    if (cntFV == 8) {
      cntFV = 0
      cntVT += 1
      if (cntVT == 30) {
        cntVT = 0
        cntV += 1
        cntV %= 2
        curNt = ntable1((cntV<<1) + cntH)
      } else if (cntVT == 32) cntVT = 0

      // Invalidate fetched data:
      validTileData = false
    }
  }

  /** Renders the sprite data for a given scanline */
  def renderSpritesPartially(startScan: Int, scanCount: Int, bgPri: Boolean): Unit = {
    if (f_spVisibility == 1) {
      for (i <- 0 until 64) {
        if (bgPriority(i) == bgPri && sprX(i) >= 0 && sprX(i) < 256 && sprY(i)+8 >= startScan &&  sprY(i) < startScan + scanCount) {
          // Show sprite.
          if (f_spriteSize == 0) {
            // 8x8 sprites
            var srcy1 : Int = 0
            var srcy2 : Int = 8

            if (sprY(i) < startScan) srcy1 = startScan - sprY(i)-1
            if (sprY(i)+8 > startScan + scanCount) srcy2 = startScan + scanCount - sprY(i)+1
            if (f_spPatternTable == 0) {
              ptTile(sprTile(i)).render(buffer, 0, srcy1, 8, srcy2, sprX(i), sprY(i)+1, sprCol(i), sprPalette, horiFlip(i), vertFlip(i), i, pixrendered)
            } else {
              ptTile(sprTile(i)+256).render(buffer, 0, srcy1, 8, srcy2, sprX(i), sprY(i)+1, sprCol(i), sprPalette, horiFlip(i), vertFlip(i), i, pixrendered)
            }
          } else {
            // 8x16 sprites
            var top : Int = sprTile(i)

            if ((top&1)!=0) top = sprTile(i) - 1 + 256

            var srcy1 : Int = 0
            var srcy2 : Int = 8

            if (sprY(i) < startScan) srcy1 = startScan - sprY(i) - 1
            if (sprY(i)+8 > startScan + scanCount) srcy2 = startScan + scanCount - sprY(i)

            ptTile(top + (if (vertFlip(i)) 1 else 0)).render(buffer, 0, srcy1, 8, srcy2, sprX(i), sprY(i)+1, sprCol(i), sprPalette, horiFlip(i), vertFlip(i), i, pixrendered)

            srcy1 = 0
            srcy2 = 8

            if (sprY(i)+8 < startScan) srcy1 = startScan - (sprY(i) + 8 + 1)
            if (sprY(i)+16 > startScan + scanCount) srcy2 = startScan + scanCount - (sprY(i)+8)

            ptTile(top + (if (vertFlip(i)) 0 else 1)).render(buffer, 0, srcy1, 8, srcy2, sprX(i), sprY(i) + 1 + 8, sprCol(i), sprPalette, horiFlip(i), vertFlip(i), i, pixrendered)
          }
        }
      }
    }
  }

  /** Checks sprite 0 hit for given scanline. Hit depends on sprite size. */
  def checkSprite0(scan: Int): Boolean = {
    spr0HitX = -1
    spr0HitY = -1

    var toffset : Int = -1
    var tIndexAdd : Int = (if (f_spPatternTable == 0) 0 else 256)
    var t: Tile = null
    var bufferIndex : Int = -1

    var x: Int = sprX(0)
    var y: Int = sprY(0) + 1

    if (f_spriteSize == 0) { // 8x8 sprites.
      // Check range:
      if (y <= scan && y + 8 > scan && x >= -7 && x < 256) { // Sprite is in range.
        // Draw scanline:
        t = ptTile(sprTile(0) + tIndexAdd)

        if (vertFlip(0)) toffset = 7 - (scan - y)
        else toffset = scan - y

        toffset *= 8
        bufferIndex = scan * 256 + x

        if (horiFlip(0)) {
          for (i <- 7 to 0 by -1) {
            if (x >= 0 && x < 256) {
              if (bufferIndex >= 0 && bufferIndex < 61440 && pixrendered(bufferIndex) != 0) {
                if (t.pix(toffset+i) != 0) {
                  spr0HitX = bufferIndex % 256
                  spr0HitY = scan
                  return true
                }
              }
            }
            x += 1
            bufferIndex += 1
          }
        } else {
          for (i <- 0 until 8) {
            if (x >= 0 && x < 256) {
              if (bufferIndex >= 0 && bufferIndex < 61440 && pixrendered(bufferIndex) != 0) {
                if (t.pix(toffset + i) != 0) {
                  spr0HitX = bufferIndex % 256
                  spr0HitY = scan
                  return true
                }
              }
            }
            x += 1
            bufferIndex += 1
          }
        }
      }
    } else { // 8x16 sprites:
      // Check range:
      if (y <= scan && y + 16 > scan && x >= -7 && x < 256) { // Sprite is in range.
        // Draw scanline:
        if (vertFlip(0)) toffset = 15-(scan-y)
        else toffset = scan-y

        if (toffset < 8) { // first half of sprite.
          t = ptTile(sprTile(0) + (if (vertFlip(0)) 1 else 0) + (if ((sprTile(0)&1) != 0) 255 else 0))
        } else { // second half of sprite.
          t = ptTile(sprTile(0) + (if (vertFlip(0)) 0 else 1) + (if ((sprTile(0)&1) != 0) 255 else 0))
          if (vertFlip(0)) toffset = 15 - toffset
          else toffset -= 8
        }

        toffset *= 8
        bufferIndex = scan * 256 + x

        if (horiFlip(0)) {
          for ( i <- 7 to 0 by -1) {
            if (x >= 0 && x < 256) {
              if (bufferIndex >= 0 && bufferIndex < 61440 && pixrendered(bufferIndex) != 0) {
                if (t.pix(toffset+i) != 0) {
                  spr0HitX = bufferIndex % 256
                  spr0HitY = scan
                  return true
                }
              }
            }

            x += 1
            bufferIndex += 1
          }
        } else {
          for (i <- 0 until 8) {
            if (x >= 0 && x < 256) {
              if (bufferIndex >= 0 && bufferIndex < 61440 && pixrendered(bufferIndex) != 0) {
                if (t.pix(toffset + i) != 0) {
                  spr0HitX = bufferIndex % 256
                  spr0HitY = scan
                  return true
                }
              }
            }
          x += 1
          bufferIndex += 1
        }
      }
    }
  }
  return false
  }

  /** Writes to PPU memory, and updates internally buffered data appropriately. */
  def writeMem(address: Int, value: Int): Unit = {
    vramMem(address) = value

    // Update internally buffered data:
    if (address < 0x2000) {
      vramMem(address) = value
      patternWrite(address,value)
    } else if (address >= 0x2000 && address < 0x23c0) {
      nameTableWrite(ntable1(0), address - 0x2000, value)
    } else if (address >= 0x23c0 && address < 0x2400) {
      attribTableWrite(ntable1(0), address-0x23c0, value)
    } else if (address >= 0x2400 && address < 0x27c0) {
      nameTableWrite(ntable1(1), address-0x2400, value)
    } else if (address >= 0x27c0 && address < 0x2800) {
      attribTableWrite(ntable1(1), address-0x27c0, value)
    } else if (address >= 0x2800 && address < 0x2bc0) {
      nameTableWrite(ntable1(2), address-0x2800, value)
    } else if (address >= 0x2bc0 && address < 0x2c00) {
      attribTableWrite(ntable1(2), address-0x2bc0, value)
    } else if (address >= 0x2c00 && address < 0x2fc0) {
      nameTableWrite(ntable1(3), address-0x2c00, value)
    } else if (address >= 0x2fc0 && address < 0x3000) {
      attribTableWrite(ntable1(3), address - 0x2fc0, value)
    } else if (address >= 0x3f00 && address < 0x3f20) {
      updatePalettes()
    }
  }

  /** Reads data from $3f00 to $f20 into the two buffered palettes. */
  def updatePalettes(): Unit = {
    for (i <- 0 until 16) {
      if (f_dispType == 0) imgPalette(i) = palTable.getEntry(vramMem(0x3f00 + i) & 63)
      else imgPalette(i) = palTable.getEntry(vramMem(0x3f00 + i) & 32)
    }

    for (i <- 0 until 16) {
      if (f_dispType == 0) sprPalette(i) = palTable.getEntry(vramMem(0x3f10 + i) & 63)
      else sprPalette(i) = palTable.getEntry(vramMem(0x3f10 + i) & 32)
    }
  }

  /** Updates the internal pattern table buffers with new given value. */
  def patternWrite(address: Int, value: Int): Unit = {
    var tileIndex : Int = address / 16
    var leftOver : Int = address % 16

    if (leftOver < 8) ptTile(tileIndex).setScanline(leftOver, value, vramMem(address + 8))
    else ptTile(tileIndex).setScanline(leftOver - 8, vramMem(address - 8), value)
  }

  /** Updates the internal name table buffers with this new byte. */
  def nameTableWrite(index: Int, address: Int, value: Int): Unit = {
    nameTable(index).tile(address) = value

    // Update Sprite #0 hit:
    checkSprite0(scanline - 20)
  }

  /** Updates the internal pattern table buffers with this new attribute table byte. */
  def attribTableWrite(index: Int, address: Int, value: Int): Unit = nameTable(index).writeAttrib(address, value)

  /** Updates the internally buffered sprite data with this new byte of info. */
  def spriteRamWriteUpdate(address: Int, value: Int): Unit = {
    var tIndex : Int = Math.floor(address / 4).asInstanceOf[Int]

    if (tIndex == 0) checkSprite0(scanline - 20)

    if (address % 4 == 0) sprY(tIndex) = value // Y coordinate
    else if (address % 4 == 1) sprTile(tIndex) = value // Tile index
    else if (address % 4 == 2) {
      // Attributes
      vertFlip(tIndex) = ((value & 0x80) != 0)
      horiFlip(tIndex) = ((value & 0x40) !=0 )
      bgPriority(tIndex) = ((value & 0x20) != 0)
      sprCol(tIndex) = (value & 3) << 2
    }
    else if (address % 4 == 3) sprX(tIndex) = value // X coordinate
  }

  /** Does a non-maskable interrupt. */
  def doNMI(): Unit = {
    // Set VBlank flag:
    setStatusFlag(StatusVBlank, true)
    nes.cpu.requestIrq(1)
  }

  // TODO if needed to and from JSON
}

object PPU {

  /** The NameTable will contain two arrays:
    * - Tile:
    * 			Each byte in this array controls one 8x8 pixel character cell, and each nametable has 30 rows of 32 tiles each.
    *
    * - Attribute (Attrib):
    *			The attribute table is a 64-byte array at the end of each nametable that controls which palette is assigned to each part of the background.
    *
    * A NES will contain 4 NameTables, arranged in a 2x2 pattern.
    */
  class NameTable(w: Int, h: Int, n: String) {
    val width : Int = w
    val height : Int = h
    val name : String = n

    // Initially filled with 0's.
    var tile : Array[Int] = Array.fill(width*height)(0)
    /** Controls which palette is assigned to each part of the background.  */
    var attrib : Array[Int] = Array.fill(width*height)(0)

    /** Returns the searched tile. */
    def getTileIndex(x: Int, y: Int): Int = tile(y*width+x)

    /** Returns the searched attribute. */
    def getAttrib(x: Int, y: Int): Int = attrib(y*width+x)

    /** Writes given value in given index in the attributes table.
      * Note that the for loop (and bit operations) is necessary to select only what we want to change, as each byte in the attribute table controls a palette of a 32x32 pixel.
      */
    def writeAttrib(index: Int, value: Int): Unit = {
      val basex: Int = (index % 8) * 4
      val basey : Int = (index / 8) * 4
      var add : Int = 0
      var tx, ty : Int = 0
      var attindex : Int = 0

      //var sqy, sqx : Int = 0
      for (sqy <- 0 until 2; sqx <- 0 until 2) {
        add = (value>>(2*(sqy*2+sqx)))&3 // Bit operators
        for (y <- 0 until 2; x <- 0 until 2) {
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
    *	    Will contain the corresponding emphasised color for each color of the palette.
    *
    * - Emphasis (currentEmph and emph):
    *      The color emphasis will be used to change the luminance amd chrominance phasor of each color, by using the RGB components.
    *
    * Each NES has only one color Palette.
    */
  class PaletteTable {
    var curTable : Array[Int] = new Array[Int](64)
    var emphTable : Array[Array[Int]] = Array.ofDim[Int](8, 64)
    var currentEmph : Int = -1

    /** Resets the Palette */
    def reset(): Unit = setEmphasis(0)

    /** Loads the NTSC Palette */
    def loadNTSCPalette(): Unit = {
      curTable = Array(0x525252,
          0xB40000,
          0xA00000,
          0xB1003D,
          0x740069,
          0x00005B,
          0x00005F,
          0x001840,
          0x002F10,
          0x084A08,
          0x006700,
          0x124200,
          0x6D2800,
          0x000000,
          0x000000,
          0x000000,
          0xC4D5E7,
          0xFF4000,
          0xDC0E22,
          0xFF476B,
          0xD7009F,
          0x680AD7,
          0x0019BC,
          0x0054B1,
          0x006A5B,
          0x008C03,
          0x00AB00,
          0x2C8800,
          0xA47200,
          0x000000,
          0x000000,
          0x000000,
          0xF8F8F8,
          0xFFAB3C,
          0xFF7981,
          0xFF5BC5,
          0xFF48F2,
          0xDF49FF,
          0x476DFF,
          0x00B4F7,
          0x00E0FF,
          0x00E375,
          0x03F42B,
          0x78B82E,
          0xE5E218,
          0x787878,
          0x000000,
          0x000000,
          0xFFFFFF,
          0xFFF2BE,
          0xF8B8B8,
          0xF8B8D8,
          0xFFB6FF,
          0xFFC3FF,
          0xC7D1FF,
          0x9ADAFF,
          0x88EDF8,
          0x83FFDD,
          0xB8F8B8,
          0xF5F8AC,
          0xFFFFB0,
          0xF8D8F8,
          0x000000,
          0x000000)
      makeTables()
      setEmphasis(0)
    }

    /** Loads the PAL Palette */
    def loadPALPalette(): Unit = {
      curTable = Array(0x525252,
          0xB40000,
          0xA00000,
          0xB1003D,
          0x740069,
          0x00005B,
          0x00005F,
          0x001840,
          0x002F10,
          0x084A08,
          0x006700,
          0x124200,
          0x6D2800,
          0x000000,
          0x000000,
          0x000000,
          0xC4D5E7,
          0xFF4000,
          0xDC0E22,
          0xFF476B,
          0xD7009F,
          0x680AD7,
          0x0019BC,
          0x0054B1,
          0x006A5B,
          0x008C03,
          0x00AB00,
          0x2C8800,
          0xA47200,
          0x000000,
          0x000000,
          0x000000,
          0xF8F8F8,
          0xFFAB3C,
          0xFF7981,
          0xFF5BC5,
          0xFF48F2,
          0xDF49FF,
          0x476DFF,
          0x00B4F7,
          0x00E0FF,
          0x00E375,
          0x03F42B,
          0x78B82E,
          0xE5E218,
          0x787878,
          0x000000,
          0x000000,
          0xFFFFFF,
          0xFFF2BE,
          0xF8B8B8,
          0xF8B8D8,
          0xFFB6FF,
          0xFFC3FF,
          0xC7D1FF,
          0x9ADAFF,
          0x88EDF8,
          0x83FFDD,
          0xB8F8B8,
          0xF5F8AC,
          0xFFFFB0,
          0xF8D8F8,
          0x000000,
          0x000000)
      makeTables()
      setEmphasis(0)
    }

    /** Creates the emphasis table for each existing emphasis (0 to 8), using the given Palette (NTSC, PAL or default) */
    def makeTables(): Unit = {
      var r, g, b, col, i : Int = 0
      var rFactor, gFactor, bFactor : Double = 1.0

      // Calculate a table for each possible emphasis
      for (emph <- 0 until 8) {
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
        for (i <- 0 until 64) {
          col = curTable(i)
          r = (getRed(col) * rFactor).toInt
          g = (getGreen(col) * gFactor).toInt
          b = (getBlue(col) * bFactor).toInt
          emphTable(emph)(i) = getRgb(r,g,b)
        }
      }
    }

    /** Sets the current emphasis to given value. */
    def setEmphasis(emph: Int): Unit = {
      if (emph != currentEmph) {
        currentEmph = emph
        var i : Int = 0
        for (i <- 0 until 64) curTable(i) = emphTable(emph)(i)
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
      curTable = Array(getRgb(117,117,117),
          getRgb(39,27,143),
          getRgb(0,0,171),
          getRgb(71,0,159),
          getRgb(143,0,119),
          getRgb(171,0,19),
          getRgb(167,0,0),
          getRgb(127,11,0),
          getRgb(67,47,0),
          getRgb(0,71,0),
          getRgb(0,81,0),
          getRgb(0,63,23),
          getRgb(27,63,95),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(188,188,188),
          getRgb(0,115,239),
          getRgb(35,59,239),
          getRgb(131,0,243),
          getRgb(191,0,191),
          getRgb(231,0,91),
          getRgb(219,43,0),
          getRgb(203,79,15),
          getRgb(139,115,0),
          getRgb(0,151,0),
          getRgb(0,171,0),
          getRgb(0,147, 59),
          getRgb(0,131,139),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(255,255,255),
          getRgb(63,191,255),
          getRgb(95,151,255),
          getRgb(167,139,253),
          getRgb(247,123,255),
          getRgb(255,119,183),
          getRgb(255,119,99),
          getRgb(255,155,59),
          getRgb(243,191,63),
          getRgb(131,211,19),
          getRgb(79,223,75),
          getRgb(88,248,152),
          getRgb(0,235,219),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(255,255,255),
          getRgb(171,231,255),
          getRgb(199,215,255),
          getRgb(215,203,255),
          getRgb(255,199,219),
          getRgb(255,191,179),
          getRgb(255,219,171),
          getRgb(255,231,163),
          getRgb(227,255,163),
          getRgb(171,243,191),
          getRgb(179,255,207),
          getRgb(159,255,243),
          getRgb(0,0,0),
          getRgb(0,0,0),
          getRgb(0,0,0))

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
  class Tile {
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
      for (y <- 0 until 8) setScanline(y, scanlineArray(y), scanlineArray(y+8))
    }

    /** Sets the scanline, by initializing the pix data */
    def setScanline(sline: Int, b1: Int, b2: Int): Unit = {
      var initialized : Boolean = true
      var tIndex : Int = sline<<3
      var x : Int = 0

      for (x <- 0 until 8) {
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
        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
            renderFunction(buffer, palette, palAdd, x, y, pri, priTable)
            fbIndex += 1
            tIndex += 1
          }
          fbIndex -= 8
          fbIndex += 256
        }

      } else if (flipHorizontal && !flipVertical) { // Mirrored tile
        tIndex = 7
        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
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

        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
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

        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
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

}
