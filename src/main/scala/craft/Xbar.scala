package craft

import Chisel._
import cde._
import junctions._
import uncore.tilelink._
import uncore.converters._
import rocketchip.{ExtMemSize, PeripheryUtils}

case object InPorts extends Field[Int]
case object OutPorts extends Field[Int]

class CraftXBar(topParams: Parameters) extends Module {
  implicit val p = topParams

  val io = new Bundle {
    val in = Vec(p(InPorts), new NastiIO).flip
    val out = Vec(p(OutPorts), new NastiIO)
  }

  val inPorts = p(InPorts)
  val outPorts = p(OutPorts)
  val memSize = p(ExtMemSize)

  val addrMap = new AddrMap(Seq.tabulate(inPorts) { i =>
    AddrMapEntry(s"chan$i", MemSize(memSize / outPorts, MemAttr(AddrMapProt.RWX)))
  })

  val bus = Module(new TileLinkRecursiveInterconnect(outPorts, addrMap))

  bus.io.in.zip(io.in).foreach { case (bus, in) =>
    bus <> PeripheryUtils.convertAXItoTL(in)
  }

  io.out.zip(bus.io.out).foreach { case (out, bus) =>
    out <> PeripheryUtils.convertTLtoAXI(bus)
  }
}
