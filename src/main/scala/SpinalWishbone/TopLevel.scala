/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published :by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

import spinal.lib.bus.misc._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.wishbone.{WishboneGPIO}
import spinal.lib.io.{TriStateArray, TriState}
import spinal.lib.wishbone.sim._
import spinal.lib.wishbone.sim.WishboneSim._
//import spinal.lib.bus.wishbone.{Wishbone,WishboneConfig,WishboneSlaveFactory}
import spinal.lib.bus.wishbone.Wishbone._
import spinal.core.sim._
import spinal.core._
import spinal.sim._

/*class MyTopLevel extends Component {
  val io = new Bundle {
    val bus = slave (Wishbone(WishboneConfig(8,8)))
    val wtitten = out Bits(8 bits)
  }

  val ctl = new WishboneSlaveFactory(Wishbone(WishboneConfig(8,8)))
  val rega, regb = Reg(UInt(8 bits)) init(44)
  ctl.readAndWrite(rega, address = 9)

  ctl.onWrite(9){
    rega := 11
  }

}*/
class yosys_assert(ceck : Bool) extends BlackBox{
  //val io = new Bundle{
  //  val ceck = in Bool
  //}
  addGeneric("ceck", ceck)
}


class Wishbonetest extends Component {
  val io = new Bundle{
    val bus = slave(Wishbone(WishboneConfig(8,8,useCYC = true)))
    //val bus_out = master(Wishbone(WishboneConfig(8,8,useCYC = true)))
    //val data_in = in Bits(8 bits)
    val data_out = out Bits(8 bits)
  }

  val feed = RegNext(io.bus.CYC && io.bus.STB) init(False)
  io.bus.ACK := feed && io.bus.STB
  val value = RegNextWhen(io.bus.DAT_MOSI, io.bus.CYC && io.bus.STB) init(0)
  io.data_out := value
  io.bus.DAT_MISO := value
 //  val dec = new WishboneDecoder(io.bus.getConfig,((0x40000,0x0001)))
 // io.bus_out <> dec.io.output
 // io.bus <> dec.io.input
 // val prova = new yosys_assert(io.bus.ACK)
  //io.bus.DAT_MISO := 0
  //io.bus.ACK := False
  //Wishbone.SlaveRead(io.bus, io.data_in)
  //io.data_out := Delay(io.data_in, 1)
  //Wishbone.SlavePipelinedRead(io.bus, io.data_in)
  //when(io.bus.WE){
  //  io.data_out := Wishbone.SlaveWrite(io.bus).data
  //}.otherwise{
  //Wishbone.SlavePipelinedRead(io.bus, io.data_in)
  //}
}

class WishboneDec extends Component {
  val io = new Bundle{
    val wbm = slave(Wishbone(WishboneConfig(32,32,useCYC = true)))
    val wbs = Vec(master(Wishbone(WishboneConfig(32,32,useCYC = true))),4)
    //val wbs2 = master(Wishbone(WishboneConfig(32,32,useCYC = true)))
    //val wbs3 = master(Wishbone(WishboneConfig(32,32,useCYC = true)))
    //val wbs4 = master(Wishbone(WishboneConfig(32,32,useCYC = true)))
  }
  val dummyReg = Reg(UInt(2 bits))
  val dec = new WishboneDecoder(WishboneConfig(32,32,useCYC = true),Seq((0x4000,4 kB),(0x5000,4 kB),(0x6000,4 kB),(0x7000,4 kB)))

  dec.io.input <> io.wbm
  dec.io.outputs <> io.wbs
}

class WishboneArb extends Component {
  val io = new Bundle{
    val wbs = master(Wishbone(WishboneConfig(32,32,useCYC = true)))
    val wbm = Vec(slave(Wishbone(WishboneConfig(32,32,useCYC = true))),4)
    //val wbm2 = slave(Wishbone(WishboneConfig(32,32,useCYC = true)))
    //val wbm3 = slave(Wishbone(WishboneConfig(32,32,useCYC = true)))
    //val wbm4 = slave(Wishbone(WishboneConfig(32,32,useCYC = true)))
  }
  val dummyReg = Reg(UInt(2 bits))
  val arb = new WishboneArbiter(WishboneConfig(32,32,useCYC = true),4,4)
  arb.io.inputs <> io.wbm
  arb.io.output <> io.wbs
}

class WishboneGp extends Component {
  val io = new Bundle{
    //val clk = in Bool
    //val reset = in Bool
    val bus = slave(Wishbone(WishboneConfig(8,8,useCYC = true)))
    val gpio = master(TriStateArray(8 bits))
  }
  val dummyReg = Reg(UInt(2 bits))
  //val area = new ClockingArea(ClockDomain(clock = io.clk, reset = io.reset)){
    val gpio = new WishboneGPIO(WishboneConfig(8,8,useCYC = true),8)
    io.bus <> gpio.io.wb
    io.gpio <> gpio.io.gpio
    val gpio_out = Reg(io.gpio.read)
  //}
}

class WishboneInt extends Component{
  val io = new Bundle{
    val masters = Vec(slave(Wishbone(WishboneConfig(8,8,useCYC = true))),3)
    val slaves = Vec(master(Wishbone(WishboneConfig(8,8,useCYC = true))),4)
    //val masters = slave(Wishbone(WishboneConfig(8,8,useCYC = true)))
    //val slaves = master(Wishbone(WishboneConfig(8,8,useCYC = true)))
  }
  val intercon = new WishboneInterconFactory()  
  intercon.addMaster(io.masters(0))
  intercon.addMaster(io.masters(1))
  intercon.addMaster(io.masters(2))

  intercon.addSlave(io.slaves(0),SizeMapping(0x00, 2))
  intercon.addSlave(io.slaves(1),SizeMapping(0x04, 2))
  intercon.addSlave(io.slaves(2),SizeMapping(0x08, 2))
  intercon.addSlave(io.slaves(3),SizeMapping(0x0C, 2))

  intercon.build()
}

class WishboneInt2 extends Component{
  val io = new Bundle{
    val masters = Vec(slave(Wishbone(WishboneConfig(8,8,useCYC = true))),1)
    val slaves = Vec(master(Wishbone(WishboneConfig(8,8,useCYC = true))),2)
  }
  val intercon = new WishboneInterconFactory()  
  for(mast <- io.masters){
    intercon.addMaster(mast)
  }
  for((slav,num) <- (io.slaves).zipWithIndex){
    intercon.addSlave(slav,SizeMapping(num*4, 4))
  }
  //intercon.addMaster(io.masters(0))
  //intercon.addSlave(io.slaves(0),SizeMapping(0x00, 2))
  //intercon.addSlave(io.slaves(1),SizeMapping(0x10, 2))

  val inte = intercon.build()
}
object MyTopLevel {
  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new MyTopLevel)
    //SpinalVerilog(new Wishbonetest)
    //SpinalConfig().dumpWave(vcdPath="waveform.vcd").generateVerilog(new WishboneIntercon)
    //SpinalConfig().dumpWave(vcdPath="waveform.vcd").generateVerilog(new WishboneArb)
    //SpinalConfig().dumpWave(vcdPath="waveform.vcd").generateVerilog(new ClockingArea(){WishboneGPIO(WishboneConfig(32,32,useCYC = true),8)})
  //  SpinalConfig().dumpWave(vcdPath="waveform.vcd").generateVerilog(new WishboneGp)


//    SimConfig(rtl = new Wishbonetest).withWave.doManagedSim{ dut =>
//      dut.io.bus.STB #= false
//      dut.io.bus.CYC #= false
//      dut.clockDomain.forkStimulus(period = 10)
//      dut.clockDomain.waitRisingEdge()
//      val trs = List( WishboneMasterTransaction(42,41,true),
//                      WishboneMasterTransaction(43,42,true),
//                      WishboneMasterTransaction(45,43,true),
//                      WishboneMasterTransaction(46,44,false),
//                      WishboneMasterTransaction(47,40,false)
//                      )
//      val wbm = new WishboneSim(dut,dut.io.bus)
//      wbm.sendAsMaster(trs)
//      sleep(10)
//    }
//
//    SimConfig(rtl = new WishboneDec).withWave.doManagedSim("WbDecoder"){ dut =>
//      dut.io.wbm.STB #= false
//      dut.io.wbm.CYC #= false
//      dut.io.wbs(0).ACK #= false
//      dut.io.wbs(1).ACK #= false
//
//      dut.clockDomain.forkStimulus(period = 2)
//      dut.clockDomain.waitRisingEdge()
//      val trs1 = List( WishboneMasterTransaction(BigInt("4000",16),41,true) )
//      val trs2 = List( WishboneMasterTransaction(BigInt("5000",16),42,true) )
//
//      val wbm = new WishboneSim(dut,dut.io.wbm)
//      val wbs0 = new WishboneSim(dut,dut.io.wbs(0))
//      val wbs1 = new WishboneSim(dut,dut.io.wbs(1))
//      val master = fork {
//      val _ = wbm.sendAsMaster(trs1)
//      val _1 = wbm.sendAsMaster(trs2)
//      }
//
//      val slave1 = fork{val _ = wbs0.receiveAsSlave(WishboneSlaveTransaction(BigInt("4000",16),1,true))}
//      val slave2 = fork{val _ = wbs1.receiveAsSlave(WishboneSlaveTransaction(BigInt("5000",16),1,true))}
//
//      master.join()
//      slave1.join()
//      slave2.join()
//      sleep(10)
//    }
//
//    SimConfig(rtl = new WishboneArb).withWave.doManagedSim("WbArbiter"){ dut =>
//      for (wm <- dut.io.wbm){
//        wm.CYC #= false
//        wm.STB #= false
//      }
//      dut.io.wbs.ACK #= false
//      dut.clockDomain.forkStimulus(period = 2)
//      dut.clockDomain.waitRisingEdge()
//      val wbs = new WishboneSim(dut,dut.io.wbs)
//      val wbm1 = new WishboneSim(dut,dut.io.wbm(0))
//      val wbm2 = new WishboneSim(dut,dut.io.wbm(1))
//      val wbm3 = new WishboneSim(dut,dut.io.wbm(2))
//      val wbm4 = new WishboneSim(dut,dut.io.wbm(3))
//
//      val master1 = fork{val _ = wbm1.sendAsMaster(List(WishboneMasterTransaction(BigInt("4000",16),40,true)))}
//      //sleep(100)
//      val master2 = fork{val _ = wbm2.sendAsMaster(List(WishboneMasterTransaction(BigInt("5000",16),40,true)))}
//      //sleep(100)
//      val master3 = fork{val _ = wbm3.sendAsMaster(List(WishboneMasterTransaction(BigInt("6000",16),40,true)))}
//      //sleep(100)
//      val master4 = fork{val _ = wbm4.sendAsMaster(List(WishboneMasterTransaction(BigInt("7000",16),40,true)))}
//
//      val slave1 = fork{val _ = wbs.receiveAsSlave(WishboneSlaveTransaction(BigInt("4000",16),1,true))}
//      val slave2 = fork{val _ = wbs.receiveAsSlave(WishboneSlaveTransaction(BigInt("5000",16),1,true))}
//      val slave3 = fork{val _ = wbs.receiveAsSlave(WishboneSlaveTransaction(BigInt("6000",16),1,true))}
//      val slave4 = fork{val _ = wbs.receiveAsSlave(WishboneSlaveTransaction(BigInt("7000",16),1,true))}
//      master1.join()
//      master2.join()
//      master3.join()
//      master4.join()
//      slave1.join()
//      slave2.join()
//      slave3.join()
//      slave4.join()
//      sleep(100)
//    }
//
//    SimConfig(rtl= new WishboneGp).withWave.doManagedSim("WbGPIO"){ dut =>
//      dut.io.bus.STB #= false
//      dut.io.bus.CYC #= false
//      dut.io.bus.DAT_MOSI #= 0
//      dut.io.bus.DAT_MISO #= 0
//      dut.io.bus.WE #= false
//      dut.io.bus.ADR #= 0
//      dut.io.gpio.read #= 0
//      dut.io.gpio.write #= 0
//      dut.io.gpio.writeEnable #= 0
//      dut.clockDomain.forkStimulus(period = 2)
//      dut.clockDomain.waitRisingEdge()
//      val wbm = new WishboneSim(dut,dut.io.bus)
//      val master1 = fork{val _ = wbm.sendAsMaster(List( WishboneMasterTransaction(BigInt("0004",16),16,true),
//                                                        WishboneMasterTransaction(BigInt("0004",16),0,false),
//                                                        WishboneMasterTransaction(BigInt("0000",16),0,false),
//                                                        WishboneMasterTransaction(BigInt("0004",16),44,false)))}
//      master1.join()
//      sleep(10)
//    }

  //  SimConfig(rtl = new WishboneInt).withWave.doManagedSim{ dut =>
  //    
  //    dut.clockDomain.forkStimulus(period = 2)
  //    val wbm1 = new WishboneSim(dut,dut.io.masters(0))
  //    val wbm2 = new WishboneSim(dut,dut.io.masters(1))
  //    val wbm3 = new WishboneSim(dut,dut.io.masters(2))

  //    
  //    val wbs1 = new WishboneSim(dut,dut.io.slaves(0))
  //    val wbs2 = new WishboneSim(dut,dut.io.slaves(1))
  //    val wbs3 = new WishboneSim(dut,dut.io.slaves(2))
  //    val wbs4 = new WishboneSim(dut,dut.io.slaves(3))
  //    
  //    val master1 = fork{val _ = wbm1.sendAsMaster(List( WishboneMasterTransaction(BigInt("00",16),16,true),
  //                                                      WishboneMasterTransaction(BigInt("0C",16),16,true)))}
  //    val master2 = fork{val _ = wbm2.sendAsMaster(List( WishboneMasterTransaction(BigInt("08",16),16,true)))}
  //    val master3 = fork{val _ = wbm3.sendAsMaster(List( WishboneMasterTransaction(BigInt("04",16),16,true)))}

  //  
  //  val slave1 = fork{val _ = wbs1.receiveAsSlave(WishboneSlaveTransaction(BigInt("00",16),1,true))}
  //    val slave2 = fork{val _ = wbs2.receiveAsSlave(WishboneSlaveTransaction(BigInt("04",16),1,true))}
  //    val slave3 = fork{val _ = wbs3.receiveAsSlave(WishboneSlaveTransaction(BigInt("08",16),1,true))}
  //    val slave4 = fork{val _ = wbs4.receiveAsSlave(WishboneSlaveTransaction(BigInt("0C",16),1,true))}

  //  master1.join()
  //    master2.join()
  //    master3.join()
  //  slave1.join()
  //    slave2.join()
  //    slave3.join()
  //    slave4.join()
  //  }

    //SimConfig(rtl = new WishboneInt2).withWave.doManagedSim{ dut => 
    //  dut.io.masters.foreach{ mast =>
    //    mast.ACK #= false
    //    mast.STB #= false
    //    mast.CYC #= false
    //    mast.ADR #= 0
    //  }
    //  //dut.io.master.ADR #= 0
    //  //dut.io.msr.CYC #= false
    //  //dut.io.msr.STB #= false
    //  //dut.io.msr.STB #= false
    //  dut.clockDomain.forkStimulus(period = 2)
    //  val wbm1 = new WishboneSim(dut,dut.io.masters(0))
    //  val wbm2 = new WishboneSim(dut,dut.io.masters(1))
    //  val wbs1 = new WishboneSim(dut,dut.io.slaves(0))
    //  val wbs2 = new WishboneSim(dut,dut.io.slaves(1))
    //  val wbs3 = new WishboneSim(dut,dut.io.slaves(2))

    //  
    //  val master1 = fork{val _ = wbm1.sendAsMaster(List( WishboneMasterTransaction(BigInt("00",16),16,true)))}
  ////    val master2 = fork{val _ = wbm2.sendAsMaster(List( WishboneMasterTransaction(BigInt("01",16),16,true)))}


    //  val slave1 = fork{val _ = wbs1.receiveAsSlave(WishboneSlaveTransaction(BigInt("00",16),1,true))}
    //  val slave2 = fork{val _ = wbs2.receiveAsSlave(WishboneSlaveTransaction(BigInt("08",16),1,true))}
    //  val slave3 = fork{val _ = wbs3.receiveAsSlave(WishboneSlaveTransaction(BigInt("10",16),1,true))}
    //  val slavedouble = fork{val _ = wbs3.receiveAsSlave(WishboneSlaveTransaction(BigInt("10",16),1,true))}

    //  master1.join()
   ////   master2.join()
    //  slave1.join()
    //  slave2.join()
    //  slave3.join()
    //  slavedouble.join()

    //  sleep(10)
    //} 
    SpinalVerilog(new WishboneInt2)
  }

}
