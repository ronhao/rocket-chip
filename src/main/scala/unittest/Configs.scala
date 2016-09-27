// See LICENSE for license details.

package unittest

import Chisel._
import cde.{Parameters, Config, CDEMatchError}
import rocketchip.{BaseConfig, BasePlatformConfig}

class WithJunctionsUnitTests extends Config(
  (pname, site, here) => pname match {
    case junctions.PAddrBits => 32
    case rocket.XLen => 64
    case UnitTests => (p: Parameters) => Seq(
      Module(new junctions.MultiWidthFifoTest),
      Module(new junctions.HastiTest()(p)))
    case _ => throw new CDEMatchError
  })

class JunctionsUnitTestConfig extends Config(new WithJunctionsUnitTests ++ new BasePlatformConfig)

class WithUncoreUnitTests extends Config(
  (pname, site, here) => pname match {
    case rocketchip.NCoreplexExtClients => 0
    case uncore.tilelink.TLId => "L1toL2"
    case UnitTests => (p: Parameters) => Seq(
      Module(new uncore.devices.ROMSlaveTest()(p)),
      Module(new uncore.devices.TileLinkRAMTest()(p)),
      Module(new uncore.converters.NastiConverterTest()(p)),
      Module(new uncore.tilelink2.TLFuzzRAMTest))
    case _ => throw new CDEMatchError
  }
)

class UncoreUnitTestConfig extends Config(new WithUncoreUnitTests ++ new BaseConfig)
