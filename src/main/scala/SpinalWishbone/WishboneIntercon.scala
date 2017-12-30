package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone._

import scala.collection.mutable

class WishboneInterconFactory(){
  val masters = mutable.ListBuffer[Wishbone]()
  val slaves = mutable.Map[Wishbone,SizeMapping]()

  def addSlave(wb : Wishbone, mapping : SizeMapping) : Unit = {
    slaves += (wb -> mapping)
  }

  def addMaster(wb : Wishbone) : Unit = {
    masters += wb
  }

  //def build() = new Area {
  //  def arbiters = for((slave, size) <- slaves) yield new WishboneArbiter(slave.getConfig, masters.size)
  //  def decoders = for(master <- masters) yield new WishboneDecoder(master.config, slaves.unzip._2.toList)
  // 
  //  //Assume the adress list is ordered in respect of the slaves
  //  //Interconnesion "magic"
  //  for((arbiter,count_arb) <- (arbiters).zipWithIndex){
  //    for((decoder,count_dec) <- (decoders).zipWithIndex){
  //      decoder.io.outputs(count_arb) <> arbiter.io.inputs(count_dec)
  //    }
  //  }

  //  //Partial connect
  //  for((decoder,master) <- (decoders,masters).zipped){
  //    decoder.io.input <> master
  //  }

  //  //Partial connect
  //  for((arbiter,slave) <- (arbiters,slaves).zipped){
  //    arbiter.io.output <> slave._1
  //  }
  //}
  def build() = new Area {
//    def arbiters = for(slave <- slaves.unzip._1) yield new Area{
//      val arbiter = new WishboneArbiter(slave.getConfig, masters.size)
//      arbiter.io.output <> slave
//    }
    println(masters.toString)
    println(slaves.toString)

    def decoders = for(master <- masters) yield new Area{
      val decoder = new WishboneDecoder(master.config, slaves.unzip._2.toList)
      decoder.io.input <> master
      println("decoder "+ decoder.toString +" -> "+ master.toString)

    }
    def arbiters = for(slave <- slaves.unzip._1) yield new Area{
      val arbiter = new WishboneArbiter(slave.getConfig, masters.size)
      arbiter.io.output << slave
    }

//    for((arbiter,count_arb) <- (arbiters).zipWithIndex){
//      for((decoder,count_dec) <- (decoders).zipWithIndex){
//        decoder.decoder.io.outputs(count_arb) << arbiter.arbiter.io.inputs(count_dec)
//      }
//    }
    for((arbiter,count_arb) <- (arbiters).zipWithIndex){
      for((decoder,count_dec) <- (decoders).zipWithIndex){
        decoder.decoder.io.outputs(count_arb) <> arbiter.arbiter.io.inputs(count_dec)

        println("Decoder "+ count_dec +" -> Arb "+ count_arb +" | Decoder Port "+ count_arb +" -> Arbiter Port" + count_dec )
      }
    }
//val ddd = new Area{
//decoders(0).decoder.io.outputs(0) <> arbiters.toList(0).arbiter.io.inputs(0)
//decoders(0).decoder.io.outputs(1) <> arbiters.toList(1).arbiter.io.inputs(0)
//decoders(1).decoder.io.outputs(0) <>  arbiters.toList(0).arbiter.io.inputs(1)
//decoders(1).decoder.io.outputs(1) <> arbiters.toList(1).arbiter.io.inputs(1)
//decoders(2).decoder.io.outputs(0) <> arbiters.toList(0).arbiter.io.inputs(2)
//decoders(2).decoder.io.outputs(1) <> arbiters.toList(1).arbiter.io.inputs(2)
//}

    ////Partial connect
    //for((decoder,master) <- (decoders,masters).zipped){
    //  decoder.io.input <> master
    //}

    ////Partial connect
    //for((arbiter,slave) <- (arbiters,slaves).zipped){
    //  arbiter.io.output <> slave._1
    //}
  }

}
