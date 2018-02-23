package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

case class WishboneConfig(
  val addressWidth : Int,
  val dataWidth : Int,
  val selWidth : Int = 0,
  val pipelined : Boolean = false,
  val registeredFeedback : Boolean = false,
  val useCYC : Boolean = false,
  val useSTALL : Boolean = false,
  val useLOCK : Boolean = false,
  val useERR : Boolean = false,
  val useRTY : Boolean = false,
  val tgaWidth : Int = 0,
  val tgcWidth : Int = 0,
  val tgdWidth : Int = 0,

  val useBTE : Boolean = false,
  val useCTI : Boolean = false
){
  def useTGA = tgaWidth > 0
  def useTGC = tgcWidth > 0
  def useTGD = tgdWidth > 0
  def useSEL = selWidth > 0
}

//object Wishboneconfig{
//
//  def classic(addressWidth : Int, dataWidth : Int, selWidth : Int = 0) = Wishbone(
//    addressWidth = addressWidth,
//    dataWidth = dataWidth,
//    selWidth = selWidth,
//
//
//  )
//
//  def pipelined() = WishboneConfig(
//
//    )
//
//}

case class Wishbone(config: WishboneConfig) extends Bundle with IMasterSlave {
//////////////////////////////
//FROM ZONE
//////////////////////////////
//  val RST_I = Bool
//  val CLK_I = Bool
//////////////////////////////
//COMMON
//////////////////////////////
  val DAT_MISO = Bits(config.dataWidth bits)
  val DAT_MOSI = Bits(config.dataWidth bits)
  val TGD_MISO = if(config.useTGD) Bits()  else null  //optional
  val TGD_MOSI = if(config.useTGD) Bits()  else null  //optional
//////////////////////////////
//MASTER
//////////////////////////////
  val ADR   = UInt(config.addressWidth bits)                            //ADR_O: Adress output, user for pass a binary address.
  val STB   = Bool                              //STB_O: strobe output, indicates a valid data transfer cycle.
  val ACK   = Bool                              //ACK_I: Acknoledge input, indicate normal temination cycle (view ERR_I RTY_I)
  val WE    = Bool                              //WE_O: write enable output, ndicates whether the current local bus cycle is a READ or WRITE cycle.
  val SEL   = if(config.useSEL)   Bits(config.selWidth bits)        else null    //SEL_O: select output, indicates where valid data is expected on the [DAT_I()] signal array during READ cycles, and where it is placed on the [DAT_O()] signal array during WRITE cycles.
  val CYC   = if(config.useCYC)   Bool          else null    //CYC_O: Cycle output, when asserted, indicates that a valid bus cycle is in progress.
  val STALL = if(config.useSTALL) Bool          else null    //STALL_I: Stall input, dicates that current slave is not able to accept the transfer in the transaction queue.
  val LOCK  = if(config.useLOCK)  Bool          else null    //LOCK_O: lock output,when asserted, indicates that the current bus cycle is uninterruptible.
  val ERR   = if(config.useERR)   Bool          else null    //ERR_I: indicates an abnormal cycle termination. //OPTIONAL
  val RTY   = if(config.useRTY)   Bool          else null    //RTY_I: retry input, dicates that the interface is not ready to accept or send data, \and that the cycle should be retried. //OPTIONAL
  val TGA   = if(config.useTGA)   Bits(config.tgaWidth bits)        else null
  val TGC   = if(config.useTGC)   Bits(config.tgcWidth bits)        else null
  //////////////////
  //Registered feedback
  //////////////////
  val BTE   = if(config.useBTE)   Bits()        else null        //optiona tag
  val CTI   = if(config.useCTI)   Bits(3 bits)  else null       //optional, only for continuous read

  override def asMaster(): Unit = {
    outWithNull(DAT_MOSI, TGD_MOSI, ADR, CYC, LOCK, SEL, STB, TGA, TGC, WE, CTI, BTE)
    inWithNull(DAT_MISO, TGD_MISO, ACK, STALL, ERR, RTY)
  }

  def isCycle : Bool = if(config.useERR) !ERR && CYC else CYC
  def isWrite : Bool = isCycle && WE
  def isRead : Bool  = isCycle && !WE
  def isReadCycle : Bool = isRead && STB
  def isWriteCycle : Bool = isWrite && STB
  def isStalled : Bool = if(config.pipelined) isCycle && STALL else False
  def isAcknoledge : Bool = isCycle && ACK
  def isStrobe : Bool = isCycle && STB

  def getConfig = config

  def doSlaveWrite : Bool = this.CYC && this.STB && this.WE
  def doSlaveRead : Bool = this.CYC && this.STB && !this.WE
  def doSlavePipelinedWrite : Bool = this.CYC && this.WE
  def doSlavePipelinedRead : Bool = this.CYC && !this.WE

  def toDataMOSIStream : Stream[Bits] = {
    val ret = Stream(Bits(this.getConfig.dataWidth bits))
    ret.valid := this.doSlaveRead
    ret.ready := this.ACK
    ret.payload := this.DAT_MOSI
    ret
  }

  def toDataMISOStream : Stream[Bits] = {
    val ret = Stream(Bits(this.getConfig.dataWidth bits))
    ret.valid := this.doSlaveWrite
    ret.ready := this.ACK
    this.DAT_MISO := ret.payload
    ret
  }

  def connectTo(that : Wishbone, allowDataResize : Boolean = false, allowAddressResize : Boolean = false, allowTagResize : Boolean = false) : Unit = {
    this <-> that
    if(allowDataResize == false){
      this.DAT_MISO.removeAssignments()
      this.DAT_MOSI.removeAssignments()
      this.DAT_MISO.resized <> that.DAT_MISO
      this.DAT_MOSI.resized <> that.DAT_MOSI

    }
    if(allowAddressResize){
      this.ADR.removeAssignments()
      this.ADR.resized <> that.ADR
    }
    if(allowTagResize){
      this.TGA.removeAssignments()
      this.TGC.removeAssignments()
      this.TGA.resized <> that.TGA
      this.TGC.resized <> that.TGC
    }
  }

  def <-> (sink : Wishbone) : Unit = {
    assert(this.config.addressWidth >= sink.getConfig.addressWidth)
    assert(this.config.selWidth == sink.getConfig.selWidth)

    sink.ADR <> this.ADR
    sink.DAT_MOSI <> this.DAT_MOSI
    this.DAT_MISO <> sink.DAT_MISO
    sink.STB <> this.STB
    sink.WE <> this.WE
    this.ACK <> sink.ACK
    //////
    //optional
    //////
    if(this.config.selWidth != 0 && sink.getConfig.selWidth != 0)  sink.SEL <> this.SEL
    if(this.config.useCYC && sink.getConfig.useCYC) sink.CYC <> this.CYC
    //////
    //registered
    //////
    if(this.CTI != null && sink.CTI != null) sink.CTI <> this.CTI
    if(this.BTE != null && sink.BTE != null) sink.BTE <> this.BTE

    ///////////
    //TAGS
    //////////
    if(this.TGA != null && sink.TGA != null) sink.TGA <> this.TGA
    if(this.TGC != null && sink.TGC != null) sink.TGC <> this.TGC
    if(this.TGD_MISO != null && sink.TGD_MISO != null) this.TGD_MISO <> sink.TGD_MISO
    if(this.TGD_MOSI != null && sink.TGD_MOSI != null) sink.TGD_MOSI <> this.TGD_MOSI
    /////
    //optional
    /////
    if(this.STALL != null && sink.STALL != null) this.STALL <> sink.STALL
    if(this.ERR != null && sink.ERR != null) this.ERR <> sink.ERR
    if(this.RTY != null && sink.RTY != null) this.RTY <> sink.RTY
  }
}