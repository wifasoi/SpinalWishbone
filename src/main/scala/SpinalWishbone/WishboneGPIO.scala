package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}
import spinal.lib.bus.wishbone._

class WishboneGPIO(config : WishboneConfig, gpioWidth : Int) extends Component{
  val io = new Bundle{
    val wb = slave(Wishbone(config))
    val gpio = master(TriStateArray(gpioWidth bits))
  }

  val ctrl = WishboneSlaveFactory(io.wb)

  //ctrl.read(io.gpio.read, 0)
  //ctrl.write(io.gpio.write, 0)
  //ctrl.driveAndRead(io.gpio.writeEnable, 4)
  //io.gpio.writeEnable.getDrivingReg init(0)

  ctrl.read(io.gpio.read, 0)
  ctrl.driveAndRead(io.gpio.write, 4)
  ctrl.driveAndRead(io.gpio.writeEnable, 8)
  io.gpio.writeEnable.getDrivingReg init(0)
}
