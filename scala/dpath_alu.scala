// See LICENSE for license details.

package rocket

import Chisel._
import cde.{Parameters, Field}
import Instructions._

object ALU
{
  val SZ_ALU_FN = 5
  val FN_X    = BitPat("b?????")
  val FN_ADD  = UInt(0)
  val FN_SL   = UInt(1)
  val FN_SEQ  = UInt(2)
  val FN_SNE  = UInt(3)
  val FN_XOR  = UInt(4)
  val FN_SR   = UInt(5)
  val FN_OR   = UInt(6)
  val FN_AND  = UInt(7)
  val FN_SUB  = UInt(10)
  val FN_SRA  = UInt(11)
  val FN_SLT  = UInt(12)
  val FN_SGE  = UInt(13)
  val FN_SLTU = UInt(14)
  val FN_SGEU = UInt(15)
  val FN_VADD = UInt(16)
  val FN_VSUB = UInt(24)

  val FN_DIV  = FN_XOR
  val FN_DIVU = FN_SR
  val FN_REM  = FN_OR
  val FN_REMU = FN_AND

  val FN_MUL    = FN_ADD
  val FN_MULH   = FN_SL
  val FN_MULHSU = FN_SLT
  val FN_MULHU  = FN_SLTU

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd === FN_SEQ || cmd === FN_SNE || cmd >= FN_SLT
  def isVect(cmd: UInt) = cmd === FN_VADD || cmd === FN_VSUB
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}
import ALU._

class ALU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, 4*xLen)
    val in1 = UInt(INPUT, 4*xLen)
    val out = UInt(OUTPUT, 4*xLen)
    val adder_out = UInt(OUTPUT, 4*xLen)
    val cmp_out = Bool(OUTPUT)
  }

  // VADD, VSUB
  val vin1_0_inv = io.in1(63, 0)
  val vin1_1_inv = io.in1(127, 64)
  val vin1_2_inv = io.in1(191, 128)
  val vin1_3_inv = io.in1(255, 192)
  
  val vin2_0_inv = Mux(isSub(io.fn), ~io.in2(63, 0), io.in2(63, 0))
  val vin2_1_inv = Mux(isSub(io.fn), ~io.in2(127, 64), io.in2(127, 64))
  val vin2_2_inv = Mux(isSub(io.fn), ~io.in2(191, 128), io.in2(191, 128))
  val vin2_3_inv = Mux(isSub(io.fn), ~io.in2(255, 192), io.in2(255, 192))

  val vsum0 = vin1_0_inv + vin2_0_inv + isSub(io.fn)
  val vsum1 = vin1_1_inv + vin2_1_inv + isSub(io.fn)
  val vsum2 = vin1_2_inv + vin2_2_inv + isSub(io.fn)
  val vsum3 = vin1_3_inv + vin2_3_inv + isSub(io.fn)

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  io.cmp_out := cmpInverted(io.fn) ^
    Mux(cmpEq(io.fn), in1_xor_in2 === UInt(0),
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1))))

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).toSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, UInt(0)) |
              Mux(io.fn === FN_SL,                     shout_l, UInt(0))

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, UInt(0)) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, UInt(0))
  val shift_logic = (isCmp(io.fn) && io.cmp_out) | logic | shout
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := Mux(isVect(io.fn), Cat(vsum3, vsum2, vsum1, vsum0), out)
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
