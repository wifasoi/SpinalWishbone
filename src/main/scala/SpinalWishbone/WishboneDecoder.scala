package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

class WishboneDecoder(config : WishboneConfig, decodings : Seq[SizeMapping]) extends Component {
  val io = new Bundle {
    val input = slave(Wishbone(config))
    val outputs = Vec(master(Wishbone(config)),decodings.size)
  }

  io.outputs.map(_.DAT_MOSI  := 0    )
  io.outputs.map(_.ADR       := 0    )
  io.outputs.map(_.STB       := False)
  io.outputs.map(_.WE        := False)
  if(config.useSEL)   io.outputs.map(_.SEL      := 0     )
  if(config.useCYC)   io.outputs.map(_.CYC      := False )
  if(config.useTGD)   io.outputs.map(_.TGD_MOSI := 0     )
  if(config.useLOCK)  io.outputs.map(_.LOCK     := False )
  if(config.useTGA)   io.outputs.map(_.TGA      := 0     )
  if(config.useTGC)   io.outputs.map(_.TGC      := 0     )
  if(config.useBTE)   io.outputs.map(_.BTE      := 0     )
  if(config.useCTI)   io.outputs.map(_.CTI      := 0     )

  io.input.DAT_MISO := 0
  io.input.ACK      := False
  if(config.useTGD)   io.input.TGD_MISO     := 0          //in as Master
  if(config.useSTALL) io.input.STALL        := False      //in as Master
  if(config.useERR)   io.input.ERR          := False      //in as Master
  if(config.useRTY)   io.input.RTY          := False      //in as Master

  for((slave, select) <- decodings.zipWithIndex){
    when(slave.hit(io.input.ADR) && io.input.CYC){

      io.outputs(select) <> io.input
    }
  }
}
