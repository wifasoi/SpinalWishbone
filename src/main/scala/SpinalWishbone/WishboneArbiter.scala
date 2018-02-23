//TODO: FIX size of bits (in wishbone
//TODO: FIx if nothing is selected, drive bus with zeroes
//TODO: invert order of the standard value (goes first)
package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

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
                      io.inputs.map(_.DAT_MISO.clearAll())
  if(config.useTGD)   io.inputs.map(_.TGD_MISO.clearAll())
                      io.inputs.map(_.ACK   := False)
  if(config.useSTALL) io.inputs.map(_.STALL := False)
  if(config.useERR)   io.inputs.map(_.ERR   := False)
  if(config.useRTY)   io.inputs.map(_.RTY   := False)

  val status = Vec(io.inputs.map(_.CYC))
  val select = Reg(UInt(log2Up(inputCount) bits)) init(0)

  when(!io.output.CYC){ //TODO: Check looping
    select := OHToUInt(OHMasking.roundRobin(status.asBits, B(priority)))
  }

  io.inputs(select) <> io.output
}
