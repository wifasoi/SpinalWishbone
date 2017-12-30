package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class WishboneConfig(
  val addressWidth : Int,
  val dataWidth : Int,
  val selWidth : Int = 0,
  val pipelined : Boolean = false,
  val registeredFeedback : Boolean = false,
  val useSEL : Boolean = false,
  val useCYC : Boolean = false,
  val useSTALL : Boolean = false,
  val useLOCK : Boolean = false,
  val useERR : Boolean = false,
  val useRTY : Boolean = false,
  val useTGA : Boolean = false,
  val useTGC : Boolean = false,
  val useTGD : Boolean = false,

  val useBTE : Boolean = false,
  val useCTI : Boolean = false
){
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
  //val ADR   = Bits(config.addressWidth bits)                            //ADR_O: Adress output, user for pass a binary address.
  val ADR   = UInt(config.addressWidth bits)                            //ADR_O: Adress output, user for pass a binary address.
  val STB   = Bool                              //STB_O: strobe output, indicates a valid data transfer cycle.
  val ACK   = Bool                              //ACK_I: Acknoledge input, indicate normal temination cycle (view ERR_I RTY_I)
  val WE    = Bool                              //WE_O: write enable output, ndicates whether the current local bus cycle is a READ or WRITE cycle.
  val SEL   = if(config.selWidth != 0)   Bits(config.selWidth bits)        else null    //SEL_O: select output, indicates where valid data is expected on the [DAT_I()] signal array during READ cycles, and where it is placed on the [DAT_O()] signal array during WRITE cycles.
  val CYC   = if(config.useCYC)   Bool          else null    //CYC_O: Cycle output, when asserted, indicates that a valid bus cycle is in progress.
  val STALL = if(config.useSTALL) Bool          else null    //STALL_I: Stall input, dicates that current slave is not able to accept the transfer in the transaction queue.
  val LOCK  = if(config.useLOCK)  Bool          else null    //LOCK_O: lock output,when asserted, indicates that the current bus cycle is uninterruptible.
  val ERR   = if(config.useERR)   Bool          else null    //ERR_I: indicates an abnormal cycle termination. //OPTIONAL
  val RTY   = if(config.useRTY)   Bool          else null    //RTY_I: retry input, dicates that the interface is not ready to accept or send data, \and that the cycle should be retried. //OPTIONAL
  val TGA   = if(config.useTGA)   Bits()        else null
  val TGC   = if(config.useTGC)   Bits()        else null
  //////////////////
  //Registered feedback
  /////////////////7
  val BTE   = if(config.useBTE)   Bits()        else null        //optiona tag
  val CTI   = if(config.useCTI)   Bits(3 bits)  else null       //optional, only for continuous read
/////////////////////////////
//SLAVE
/////////////////////////////
//  val ACK_O = Bool
//  val ADR_I = Bits()
//  val CYC_I = Bool
//  val STALL_O = Bool
//  val ERR_O = Bool
//  val LOCK_I = Bool
//  val RTY_O = Bool
//  val SEL_I = Bool
//  val STB_I = Bool
//  val TGA_I = Bits()
//  val TGC_I = Bits()
//  val WE_I = Bool

  override def asMaster(): Unit = {
    outWithNull(DAT_MOSI, TGD_MOSI, ADR, CYC, LOCK, SEL, STB, TGA, TGC, WE, CTI, BTE)
    inWithNull(DAT_MISO, TGD_MISO, ACK, STALL, ERR, RTY)
  }

  def >> (sink : Wishbone) : Unit = sink << this
  def << (sink : Wishbone) : Unit = {
    assert(this.config.addressWidth >= sink.getConfig.addressWidth)
    assert(this.config.selWidth == sink.getConfig.selWidth)

    sink.ADR := this.ADR
    //this.DAT_MOSI := sink.DAT_MOSI
    //sink.DAT_MISO := this.DAT_MISO
    sink.DAT_MOSI := this.DAT_MOSI
    this.DAT_MISO := sink.DAT_MISO
    sink.STB := this.STB
    sink.WE := this.WE
    this.ACK := sink.ACK
    //////
    //optional
    //////
    if(this.config.selWidth != 0 && sink.getConfig.selWidth != 0)  sink.SEL := this.SEL
    if(this.config.useCYC && sink.getConfig.useCYC) sink.CYC := this.CYC
    //////
    //registered
    //////
    if(this.CTI != null && sink.CTI != null) sink.CTI := this.CTI
    if(this.BTE != null && sink.BTE != null) sink.BTE := this.BTE

    ///////////
    //TAGS
    //////////
    if(this.TGA != null && sink.TGA != null) sink.TGA := this.TGA
    if(this.TGC != null && sink.TGC != null) sink.TGC := this.TGC
    if(this.TGD_MISO != null && sink.TGD_MISO != null) this.TGD_MISO := sink.TGD_MISO
    if(this.TGD_MOSI != null && sink.TGD_MOSI != null) sink.TGD_MOSI := this.TGD_MOSI
    /////
    //optional
    /////
    if(this.STALL != null && sink.STALL != null) this.STALL := sink.STALL
    if(this.ERR != null && sink.ERR != null) this.ERR := sink.ERR
    if(this.RTY != null && sink.RTY != null) this.RTY := sink.RTY
  }

  def getConfig = config

  def doSlaveWrite : Bool = this.CYC && this.STB && this.WE
  def doSlaveRead : Bool = this.CYC && this.STB && !this.WE
  def doSlavePipelinedWrite : Bool = this.CYC && this.WE
  def doSlavePipelinedRead : Bool = this.CYC && !this.WE
  //val DAT_MISOBundle = new Bundle {
  //  val DAT = bus.DAT_MISO
  //  val TGD = bus.TGD_MISO
  //}

  //val DAT_MOSIBundle = new Bundle {
  //  val DAT = bus.DAT_MOSI
  //  val TGD = bus.TGD_MOSI
  //}

  //val ADRBundle = new Bundle {
  //  val ADR = bus.ADR
  //  val TGA = bus.TGA
  //}

  //val CYCBundle = new Bundle {
  //  val CYC = bus.CYC
  //  val TGC = bus.TGC
  //}

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
}

object Wishbone{
  def SlaveRead(bus : Wishbone, data : Bits, TGD : Data = null) = new Area{
    bus.DAT_MISO := 0
    bus.ACK := False
    when(bus.doSlaveRead){
      bus.ACK := Delay(bus.STB, 1)
      when(bus.ACK){
        bus.DAT_MISO := data
      }
    }
  }

  def SlaveWrite(bus : Wishbone) = new Area {
    val data = Reg(Bits(bus.getConfig.dataWidth bits))
    when(bus.doSlaveWrite){
      bus.ACK := Delay(bus.STB, 1)
      data := bus.DAT_MOSI
    }
  }


  //TODO: Finish
  def SlavePipelinedRead(bus : Wishbone, data : Bits) = new Area {
    bus.DAT_MISO := 0
    bus.ACK := False
    when(bus.doSlavePipelinedRead){
      when(bus.STB){
        bus.ACK := Delay(bus.STB, 1)
        bus.DAT_MISO := data
      }
      //when(!bus.STB){
      //  bus.ACK := True
      //  bus.DAT_MISO := data
      //  when(!bus.CYC){
      //    bus.ACK := False
      //  }
      //}
        //bus.ACK := Delay(bus.STB, 1)
        //when(bus.ACK){
        //  bus.DAT_MISO := data
        //}
        //  bus.ACK := Delay(bus.STB.fall, 3)
        //val fsm = new StateMachine{
        //  val decode address : State = new State with EntryPoint{
        //    whenIsActive(goto(sendData))
        //  }

        //  val sendData : State = new State {

        //  }
        //}
    }
  }

  def MasterStandardSlavePipelined(bus : Wishbone) = new Area{
    //mus change STB
    val fsm = new StateMachine{

    }
  }

  def SlavePipelinedWrite(bus : Wishbone) = new Area {
    val data = Reg(Bits(bus.getConfig.dataWidth bits))
    when(bus.doSlavePipelinedWrite){
      when(!bus.STB){
        bus.ACK := True
        data := bus.DAT_MOSI
        when(!bus.CYC){
          bus.ACK := False
        }
      }
    }
  }

}
