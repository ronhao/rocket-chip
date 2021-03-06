// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.util.{Irrevocable, IrrevocableIO}

abstract class GenericParameterizedBundle[T <: Object](val params: T) extends Bundle
{
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(params).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throw new Exception("Unable to use GenericParameterizedBundle.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       "() takes more than one argument.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }
}

abstract class TLBundleBase(params: TLBundleParameters) extends GenericParameterizedBundle(params)

// common combos in lazy policy:
//   Put + Acquire
//   Release + AccessAck

object TLMessages 
{
  //                                  A    B    C    D    E
  val PutFullData    = UInt(0) //     .    .                   => AccessAck
  val PutPartialData = UInt(1) //     .    .                   => AccessAck
  val ArithmeticData = UInt(2) //     .    .                   => AccessAckData
  val LogicalData    = UInt(3) //     .    .                   => AccessAckData
  val Get            = UInt(4) //     .    .                   => AccessAckData
  val Hint           = UInt(5) //     .    .                   => HintAck
  val Acquire        = UInt(6) //     .                        => Grant[Data]
  val Probe          = UInt(6) //          .                   => ProbeAck[Data]
  val AccessAck      = UInt(0) //               .    .
  val AccessAckData  = UInt(1) //               .    .
  val HintAck        = UInt(2) //               .    .
//val PutThroughData = UInt(3) //               .              // future extension ?
  val ProbeAck       = UInt(4) //               .
  val ProbeAckData   = UInt(5) //               .
  val Release        = UInt(6) //               .              => ReleaseAck
  val ReleaseData    = UInt(7) //               .              => ReleaseAck
  val Grant          = UInt(4) //                    .         => GrantAck
  val GrantData      = UInt(5) //                    .         => GrantAck
  val ReleaseAck     = UInt(6) //                    .
  val GrantAck       = UInt(0) //                         .
 
  def isA(x: UInt) = x <= Acquire
  def isB(x: UInt) = x <= Probe
  def isC(x: UInt) = x <= ReleaseData
  def isD(x: UInt) = x <= ReleaseAck
}

object TLPermissions
{
  // Cap types (Grant = new permissions, Probe = permisions <= target)
  val toT = UInt(0)
  val toB = UInt(1)
  val toN = UInt(2)
  def isCap(x: UInt) = x <= toN

  // Grow types (Acquire = permissions >= target)
  val NtoB = UInt(0)
  val NtoT = UInt(1)
  val BtoT = UInt(2)
  def isGrow(x: UInt) = x <= BtoT

  // Shrink types (ProbeAck, Release)
  val TtoB = UInt(0)
  val TtoN = UInt(1)
  val BtoN = UInt(2)
  def isShrink(x: UInt) = x <= BtoN

  // Report types (ProbeAck)
  val TtoT = UInt(3)
  val BtoB = UInt(4)
  val NtoN = UInt(5)
  def isReport(x: UInt) = x <= NtoN
}

object TLAtomics
{
  // Arithmetic types
  val MIN  = UInt(0)
  val MAX  = UInt(1)
  val MINU = UInt(2)
  val MAXU = UInt(3)
  val ADD  = UInt(4)
  def isArithmetic(x: UInt) = x <= ADD

  // Logical types
  val XOR  = UInt(0)
  val OR   = UInt(1)
  val AND  = UInt(2)
  val SWAP = UInt(3)
  def isLogical(x: UInt) = x <= SWAP
}

sealed trait TLChannel extends TLBundleBase {
  val channelName: String
}
sealed trait TLDataChannel extends TLChannel
sealed trait TLAddrChannel extends TLDataChannel

final class TLBundleA(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'A' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3) // amo_opcode || perms || hint
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val addr_hi = UInt(width = params.addrHiBits) // to
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

final class TLBundleB(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'B' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val addr_hi = UInt(width = params.addrHiBits) // from
  // variable fields during multibeat:
  val mask    = UInt(width = params.dataBits/8)
  val data    = UInt(width = params.dataBits)
}

final class TLBundleC(params: TLBundleParameters)
  extends TLBundleBase(params) with TLAddrChannel
{
  val channelName = "'C' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 3)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // from
  val addr_hi = UInt(width = params.addrHiBits) // to
  val addr_lo = UInt(width = params.addrLoBits) // instead of mask
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val error   = Bool() // AccessAck[Data]
}

final class TLBundleD(params: TLBundleParameters)
  extends TLBundleBase(params) with TLDataChannel
{
  val channelName = "'D' channel"
  // fixed fields during multibeat:
  val opcode  = UInt(width = 3)
  val param   = UInt(width = 2)
  val size    = UInt(width = params.sizeBits)
  val source  = UInt(width = params.sourceBits) // to
  val sink    = UInt(width = params.sinkBits)   // from
  val addr_lo = UInt(width = params.addrLoBits) // instead of mask
  // variable fields during multibeat:
  val data    = UInt(width = params.dataBits)
  val error   = Bool() // AccessAck[Data], Grant[Data]
}

final class TLBundleE(params: TLBundleParameters)
  extends TLBundleBase(params) with TLChannel
{
  val channelName = "'E' channel"
  val sink = UInt(width = params.sinkBits) // to
}

class TLBundle(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = Irrevocable(new TLBundleA(params))
  val b = Irrevocable(new TLBundleB(params)).flip
  val c = Irrevocable(new TLBundleC(params))
  val d = Irrevocable(new TLBundleD(params)).flip
  val e = Irrevocable(new TLBundleE(params))
}

object TLBundle
{
  def apply(params: TLBundleParameters) = new TLBundle(params)
}

class IrrevocableSnoop[+T <: Data](gen: T) extends Bundle
{
  val ready = Bool()
  val valid = Bool()
  val bits = gen.asOutput

  def fire(dummy: Int = 0) = ready && valid
  override def cloneType: this.type = new IrrevocableSnoop(gen).asInstanceOf[this.type]
}

object IrrevocableSnoop
{
  def apply[T <: Data](i: IrrevocableIO[T]) = {
    val out = Wire(new IrrevocableSnoop(i.bits))
    out.ready := i.ready
    out.valid := i.valid
    out.bits  := i.bits
    out
  }
}

class TLBundleSnoop(params: TLBundleParameters) extends TLBundleBase(params)
{
  val a = new IrrevocableSnoop(new TLBundleA(params))
  val b = new IrrevocableSnoop(new TLBundleB(params))
  val c = new IrrevocableSnoop(new TLBundleC(params))
  val d = new IrrevocableSnoop(new TLBundleD(params))
  val e = new IrrevocableSnoop(new TLBundleE(params))
}

object TLBundleSnoop
{
  def apply(x: TLBundle) = {
    val out = Wire(new TLBundleSnoop(x.params))
    out.a := IrrevocableSnoop(x.a)
    out.b := IrrevocableSnoop(x.b)
    out.c := IrrevocableSnoop(x.c)
    out.d := IrrevocableSnoop(x.d)
    out.e := IrrevocableSnoop(x.e)
    out
  }
}
