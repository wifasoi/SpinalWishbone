//TODO: FIX size of bits (in wishbone
//TODO: FIx if nothing is selected, drive bus with zeroes
//TODO: invert order of the standard value (goes first)
package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._

object WishboneArbiter{
}

class WishboneArbiter(config : WishboneConfig, inputCount : Int, priority : Int = 1) extends Component{
  val io = new Bundle{
    val inputs = Vec(slave(Wishbone(config)), inputCount)
    val output = master(Wishbone(config))
  }
  /////////////
  //Otherwise//
  /////////////
                      io.inputs.map(_.DAT_MISO.clearAll())  //in as Master
  if(config.useTGD)   io.inputs.map(_.TGD_MISO.clearAll())  //in as Master
                      io.inputs.map(_.ACK       := False)  //in as Master
  if(config.useSTALL) io.inputs.map(_.STALL     := False) //in as Master
  if(config.useERR)   io.inputs.map(_.ERR       := False ) //in as Master
  if(config.useRTY)   io.inputs.map(_.RTY       := False  )//in as Master

  //val status = Vec(Bool,inputCount)
  //val status = Bits(log2Up(inputCount) bits)
  //val status = Vec(Bool,inputCount)
  val status = Vec(io.inputs.map(_.CYC))
  val select = Reg(UInt(log2Up(inputCount) bits)) init(0)
  //for((input, index) <- io.inputs.zipWithIndex){
  //  status(index) := input.CYC
  //}
  when(!io.output.CYC){ //TODO: Check looping
    select := OHToUInt(OHMasking.roundRobin(status.asBits, B(priority)))
  }
   //                     io.output.DAT_MOSI := io.inputs(select).DAT_MOSI   //out as Master
   // if(config.useTGD)   io.output.TGD_MOSI := io.inputs(select).TGD_MOSI   //out as Master
   //                     io.output.ADR      := io.inputs(select).ADR        //out as Master
   //                     io.output.STB      := io.inputs(select).STB        //out as Master
   //                     io.output.WE       := io.inputs(select).WE         //out as Master
   // if(config.useSEL)   io.output.SEL      := io.inputs(select).SEL        //out as Master
   // if(config.useCYC)   io.output.CYC      := io.inputs(select).CYC        //out as Master
   // if(config.useLOCK)  io.output.LOCK     := io.inputs(select).LOCK       //out as Master
   // if(config.useTGA)   io.output.TGA      := io.inputs(select).TGA        //out as Master
   // if(config.useTGC)   io.output.TGC      := io.inputs(select).TGC        //out as Master
   // if(config.useBTE)   io.output.BTE      := io.inputs(select).BTE        //out as Master
   // if(config.useCTI)   io.output.CTI      := io.inputs(select).CTI        //out as Master

   //                     io.inputs(select).DAT_MISO  := io.output.DAT_MISO  //in as Master
   // if(config.useTGD)   io.inputs(select).TGD_MISO  := io.output.TGD_MISO  //in as Master
   //                     io.inputs(select).ACK       := io.output.ACK       //in as Master
   // if(config.useSTALL) io.inputs(select).STALL     := io.output.STALL     //in as Master
   // if(config.useERR)   io.inputs(select).ERR       := io.output.ERR       //in as Master
   // if(config.useRTY)   io.inputs(select).RTY       := io.output.RTY       //in as Master
  io.inputs(select) << io.output
}
