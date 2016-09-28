package rocket

import Chisel._
import util.WideCounter

object ImplicitConversions {
  implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)
  implicit def intToUInt(x: Int): UInt = UInt(x)
  implicit def bigIntToUInt(x: BigInt): UInt = UInt(x)
  implicit def booleanToBool(x: Boolean): Bits = Bool(x)
  implicit def intSeqToUIntSeq(x: Seq[Int]): Seq[UInt] = x.map(UInt(_))
  implicit def wcToUInt(c: WideCounter): UInt = c.value
}
