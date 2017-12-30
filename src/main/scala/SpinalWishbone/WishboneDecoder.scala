package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._

//object WishboneDecoder{
//  def apply(master : Wishbone, slaves : Seq[(Wishbone, SizeMapping)]) : WishboneDecoder = {
//
//  }
//}

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
  if(config.useTGD)      io.input.TGD_MISO     := 0  //in as Master
  if(config.useSTALL) io.input.STALL        := False     //in as Master
  if(config.useERR)   io.input.ERR          := False      //in as Master
  if(config.useRTY)   io.input.RTY          := False      //in as Master

  for((slave, select) <- decodings.zipWithIndex){
    when(slave.hit(io.input.ADR) && io.input.CYC){
     // io.outputs(select).DAT_MOSI := io.input.DAT_MOSI      //out as Master
     // if(config.useTGD) io.outputs(select).TGD_MOSI := io.input.TGD_MOSI      //out as Master
     // io.outputs(select).ADR      := io.input.ADR           //out as Master
     // io.outputs(select).STB      := io.input.STB           //out as Master
     // io.outputs(select).WE       := io.input.WE            //out as Master
     // if(config.useSEL) io.outputs(select).SEL      := io.input.SEL           //out as Master
     // if(config.useCYC) io.outputs(select).CYC      := io.input.CYC           //out as Master
     // if(config.useLOCK) io.outputs(select).LOCK     := io.input.LOCK          //out as Master
     // if(config.useTGA) io.outputs(select).TGA      := io.input.TGA           //out as Master
     // if(config.useTGC) io.outputs(select).TGC      := io.input.TGC           //out as Master
     // if(config.useBTE) io.outputs(select).BTE      := io.input.BTE           //out as Master
     // if(config.useCTI) io.outputs(select).CTI      := io.input.CTI           //out as Master

     // io.input.DAT_MISO := io.outputs(select).DAT_MISO  //in as Master
     // if(config.useTGD)   io.input.TGD_MISO     := io.outputs(select).TGD_MISO  //in as Master
     // io.input.ACK      := io.outputs(select).ACK       //in as Master
     // if(config.useSTALL) io.input.STALL        := io.outputs(select).STALL     //in as Master
     // if(config.useERR)   io.input.ERR          := io.outputs(select).ERR       //in as Master
     // if(config.useRTY)   io.input.RTY          := io.outputs(select).RTY       //in as Master
      io.outputs(select) >> io.input
    }
  }
}
