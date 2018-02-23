package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class WishboneAdapter(wbmConfig : WishboneConfig,
                      wbsConfig: WishboneConfig,
                      allowDataResize : Boolean = false,
                      allowAddressResize : Boolean = false,
                      allowTagResize : Boolean = false) extends Component{
  val io = new Bundle {
    val wbm = slave(Wishbone(wbmConfig))
    val wbs = master(Wishbone(wbsConfig))
  }

  io.wbm.connectTo(io.wbs, allowDataResize, allowAddressResize, allowTagResize)

  (wbmConfig.pipelined,wbsConfig.pipelined) match{
    case (false,true) => {
      io.wbm.ACK.removeAssignments()
      io.wbs.STB.removeAssignments()
      val wsm = new StateMachine{
        io.wbs.STB := False
        val idle : State = new State with EntryPoint{
          whenIsActive{
            io.wbs.STB := io.wbm.STB
            when(io.wbs.STB){
              goto(wait4ack)
            }
          }
        }

        val wait4ack : State = new State{
          whenIsActive{
            when(io.wbs.ACK){
              goto(idle)
            }
          }
        }
      }
    }
    case (true, false) => {
      io.wbm.STALL.removeAssignments()
      io.wbm.STALL := io.wbs.CYC? !io.wbs.ACK | False
    }
    case _ =>
  }
}