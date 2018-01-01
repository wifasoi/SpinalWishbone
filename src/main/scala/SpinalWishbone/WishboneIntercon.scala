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

  def build() = new Area {
    val arbiters = for(slave <- slaves.unzip._1) yield new Area{
      val arbiter = new WishboneArbiter(slave.getConfig, masters.size)
      arbiter.io.output <> slave
    }

    val decoders = for(master <- masters) yield new Area{
      val decoder = new WishboneDecoder(master.config, slaves.unzip._2.toList)
      decoder.io.input <> master
      println("decoder "+ decoder.toString +" -> "+ master.toString)

    }

    for((arbiter,count_arb) <- (arbiters).zipWithIndex){
      for((decoder,count_dec) <- (decoders).zipWithIndex){
        decoder.decoder.io.outputs(count_arb) <> arbiter.arbiter.io.inputs(count_dec)

        println("Decoder "+ count_dec +" -> Arb "+ count_arb +" | Decoder Port "+ count_arb +" -> Arbiter Port" + count_dec )
      }
    }
  }
}
