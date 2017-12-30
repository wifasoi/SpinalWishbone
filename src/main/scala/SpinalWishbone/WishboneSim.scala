package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.mutable._

case class WishboneMasterTransaction( address : BigInt,
                                      data : Long,
                                      WE : Boolean,
                                      isNOP : Boolean = false,
                                      delay : Long = 0)


case class WishboneSlaveTransaction(address : BigInt,
                                    data : Long,
                                    isWrite : Boolean,
                                    delay : Long = 0)

case class WishboneMasterResponse(address : BigInt, data : Long = 0)

case class WishboneSlaveResponse( address : BigInt, data : Long)


object WishboneSim{
  def apply[T <: Component](dut : T, wb : Wishbone) : WishboneSim[T] = {
    val wbsim = new WishboneSim[T](dut,wb)
    wbsim
  }
}

class WishboneSim[T <: Component](dut : T, wb : Wishbone){
  def sendAsMaster(transaction : List[WishboneMasterTransaction]) : List[WishboneMasterResponse]@suspendable ={
    val ret = ListBuffer[WishboneMasterResponse]()
    transaction.suspendable.foreach{ tran =>
      dut.clockDomain.waitRisingEdge()
      wb.CYC #= true
      wb.STB #= true
      wb.WE #= tran.WE
      wb.ADR #= tran.address
      if(tran.WE) wb.DAT_MOSI #= tran.data
      if(!tran.WE) ret += new WishboneMasterResponse( address = tran.address, data = tran.data)

      waitUntil(wb.ACK.toBoolean)
      sleep(tran.delay)
      dut.clockDomain.waitRisingEdge()
      wb.STB #= false
    }
    wb.CYC #= false
    ret.toList
  }

  def receiveAsSlave(tran : WishboneSlaveTransaction) : List[WishboneSlaveResponse]@suspendable = {
    val ret = ListBuffer[WishboneSlaveResponse]()
    //while(wb.CYC.toBoolean && wb.ADR.toBigInt == tran.address){
    //Awhile(wb.CYC.toBoolean){
    //while(true){
      waitUntil(wb.STB.toBoolean && wb.CYC.toBoolean && wb.ADR.toBigInt == tran.address)
      dut.clockDomain.waitRisingEdge()
      wb.ACK #= true
      ret += new WishboneSlaveResponse(address = wb.ADR.toBigInt, data = wb.DAT_MOSI.toLong)
      sleep(tran.delay)
      dut.clockDomain.waitRisingEdge()
      wb.ACK #= false
    //}
    wb.ACK #= false
    ret.toList
  }
}
