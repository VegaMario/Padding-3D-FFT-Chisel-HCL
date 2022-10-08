package FloatingPointDesigns
import chisel3._
import FloatingPointDesigns.Arithmetic._
import Chisel.log2Ceil
import IEEEConversions.FPConvert._

object FPArithmetic { // you might see errors from the IDE in the FP_adders, but you can ignore them
  // parameterizable floating point adder has a register at output

  class FP_adder_v2(bw: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })
    var exponent = 0
    var mantissa = 0
    if (bw == 16) {
      exponent = 5
      mantissa = 10
    } else if (bw == 32) {
      exponent = 8
      mantissa = 23
    } else if (bw == 64) {
      exponent = 11
      mantissa = 52
    } else if (bw == 128) {
      exponent = 15
      mantissa = 112
    }

    //    val reg_in_a = Reg(UInt(bw.W))
    //    val reg_in_b = Reg(UInt(bw.W))
    //    reg_in_a := io.in_a
    //    reg_in_b := io.in_b

    // sign part of ieee number
    val sign = Wire(Vec(2, UInt(1.W)))
    sign(0) := io.in_a(bw - 1)
    sign(1) := io.in_b(bw - 1)

    // exponent part of ieee number
    val exp = Wire(Vec(2, UInt(exponent.W)))
    when(io.in_a(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) { // there is a maximum number according to IEEE 754 standards
      exp(0) := BigInt(2).pow(exponent).U - 2.U
    }.otherwise {
      exp(0) := io.in_a(bw - 2, mantissa)
    }
    when(io.in_b(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) {
      exp(1) := BigInt(2).pow(exponent).U - 2.U
    }.otherwise {
      exp(1) := io.in_b(bw - 2, mantissa)
    }

    //fractional part of ieee number
    val frac = Wire(Vec(2, UInt(mantissa.W)))
    frac(0) := io.in_a(mantissa - 1, 0)
    frac(1) := io.in_b(mantissa - 1, 0)

    // 1.0 + fractional part
    val whole_frac = Wire(Vec(2, UInt((mantissa + 1).W)))
    whole_frac(0) := 1.U ## frac(0)
    whole_frac(1) := 1.U ## frac(1)


    val out_s = Wire(UInt(1.W)) // sign of the larger input
    val out_frac = Wire(UInt(mantissa.W)) // mantissa/fractional part of larger input
    val out_exp = Wire(UInt(exponent.W)) // exponent of larger inpui
    val sub_exp = Wire(UInt(exponent.W)) // temporarily holds the result of exp(0) - exp(1) (difference in exponents)
    // just set to 0 initially
    out_exp := 0.U
    sub_exp := 0.U
    out_frac := 0.U
    out_s := 0.U

    // subtracts the exponent of second input from the first input
    val subber = Module(new full_subber(exponent))
    subber.io.in_a := exp(0)
    subber.io.in_b := exp(1)
    subber.io.in_c := 0.U

    // in case the exponent difference is negative, take the twos complement of subtraction result
    val complement = Module(new twoscomplement(exponent))
    complement.io.in := subber.io.out_s

    // add the fractional/mantissa parts of the ieee numbers together
    val adder = Module(new full_adder(mantissa + 1))
    adder.io.in_a := whole_frac(0)
    adder.io.in_b := whole_frac(1)
    adder.io.in_c := 0.U

    // these instantiations of twoscomplement are when there are negative number inputs
    val complementN_0 = Module(new twoscomplement(mantissa + 1))
    complementN_0.io.in := whole_frac(0)
    val complementN_1 = Module(new twoscomplement(mantissa + 1))
    complementN_1.io.in := whole_frac(1)

    // before we add the mantissa parts together, we need to align the exponents by shifting by the exponent difference
    val shifter = Module(new shifter(mantissa + 1))
    shifter.io.in_a := whole_frac(0)
    shifter.io.in_b := subber.io.out_s
    shifter.io.in_c := 1.U

    // if the exponent difference resulted in a borrow, then the second input has a larger exponent than the first
    when(subber.io.out_c === 1.U) {
      out_exp := exp(1)
      sub_exp := complement.io.out // the exponent difference is in the negative form, so take the twos complement
      out_s := sign(1)
      out_frac := frac(1)
      shifter.io.in_a := whole_frac(0) // shift the smaller exponent input to the right for aligning
      shifter.io.in_b := complement.io.out
      shifter.io.in_c := 1.U
      adder.io.in_a := shifter.io.out_s // now we can add the two aligned mantissa/fractional parts
      complementN_0.io.in := shifter.io.out_s
    }.otherwise { // otherwise the first input has a larger exponent or the same as the second input
      out_exp := exp(0)
      sub_exp := subber.io.out_s // the exponent difference is not negative, so just leave it alone
      out_s := sign(0)
      out_frac := frac(0)
      shifter.io.in_a := whole_frac(1) // shift the smaller exponent input to the right for aligning
      shifter.io.in_b := subber.io.out_s
      shifter.io.in_c := 1.U
      adder.io.in_b := shifter.io.out_s // now we can dd the two aligned mantissa parts
      complementN_1.io.in := shifter.io.out_s
    }

    // if first input is negative, take twos complement
    when(sign(0) === 1.U && sign(1) === 0.U) {
      adder.io.in_a := complementN_0.io.out
    }
    // if second input is negative, take twos complement, and compute mantissa addition
    when(sign(1) === 1.U && sign(0) === 0.U) {
      adder.io.in_b := complementN_1.io.out
    }

    val new_s = Wire(UInt(1.W)) // will hold the final sign result
    val new_out_frac = Wire(UInt(mantissa.W)) // will hold the final mantissa result
    val new_out_exp = Wire(UInt(exponent.W)) // will hold the final exponent result
    new_s := (~adder.io.out_c & sign(0)) | (sign(0) & sign(1)) | (~adder.io.out_c & sign(1)) // this binary equation gives the new sign of the sum
    new_out_frac := 0.U // just temporarily setting to 0
    new_out_exp := 0.U

    // this twoscomplement is here to take the two complement of the mantissa sum in case the new sign of the sum is negative
    val complementN_2 = Module(new twoscomplement(mantissa + 1))
    complementN_2.io.in := adder.io.out_s


    val D = Wire(UInt(1.W)) // will indicate in which direction the mantissa sum will have to be shifted to be correct. (D = 0 indicates current exponent needs to be increased, D= 1 indicates that the current exponent needs to be decreased)
    val E = Wire(UInt(1.W)) // will indicate if there is no need for adjusting the mantissa sum and we can leave it as it is.

    // equation for describing the conditions in which the mantissa sum will need to be shifted
    D := (~adder.io.out_c) | (sign(0) ^ sign(1))

    // equation for the conditions in which the manstissa sum does not need to be shifted. This has higher priority than D
    E := (~adder.io.out_c & ~adder.io.out_s(mantissa)) | (~adder.io.out_c & ~(sign(0) ^ sign(1))) | (adder.io.out_c & adder.io.out_s(mantissa) & (sign(0) ^ sign(1)))

    val adder_result = Wire(UInt((mantissa + 1).W))
    adder_result := adder.io.out_s
    when(new_s === 1.U && sign(0) =/= sign(1)) { // if the new sign of the sum is negative, then take the twos complement of the mantissa sum
      adder_result := complementN_2.io.out
    }

    // module finds the most significant 1 in the mantissa sum. This is used when E = 0, and D = 1, indicating a left shift
    val leadingOneFinder = Module(new leadingOneDetector(mantissa + 1))
    leadingOneFinder.io.in := adder_result
    val subber2 = Module(new full_subber(exponent))
    subber2.io.in_a := out_exp
    subber2.io.in_b := ((mantissa + 1).U - leadingOneFinder.io.out)
    subber2.io.in_c := 0.U
    when(io.in_a(bw - 2, 0) === 0.U && io.in_b(bw - 2, 0) === 0.U) {
      new_s := 0.U
      new_out_exp := 0.U
      new_out_frac := 0.U
    }.elsewhen(sub_exp >= mantissa.U) { // if the difference between the exponents is too large, larger than mantissa size.
      new_s := out_s
      new_out_frac := out_frac
      new_out_exp := out_exp
    }.elsewhen(E === 1.U) { // if the exponent should stay the same size as the largest exponent
      new_out_exp := out_exp
      new_out_frac := adder_result(mantissa - 1, 0)
    }.elsewhen(D === 0.U) { // if exponent needs to be increased by 1
      when(out_exp === BigInt(2).pow(exponent).U - 2.U) {
        new_out_exp := BigInt(2).pow(exponent).U - 2.U
        new_out_frac := BigInt(2).pow(mantissa).U - 1.U
      }.otherwise {
        new_out_exp := out_exp + 1.U
        new_out_frac := adder_result(mantissa, 1)
      }
    }.elsewhen(D === 1.U) { // if exponent needs to be decreased by 1 or more
      when(leadingOneFinder.io.out === 1.U && adder_result === 0.U && ((1.U === (sign(0) ^ sign(1)) && io.in_a(bw - 2, 0) === io.in_b(bw - 2, 0)))) {
        new_out_exp := 0.U
      }.otherwise {
        when(subber2.io.out_c === 1.U) {
          new_out_exp := 1.U(exponent.W)
          new_out_frac := BigInt(2).pow(mantissa - 1).U(mantissa.W)
        }.otherwise {
          new_out_exp := subber2.io.out_s
          new_out_frac := adder_result(mantissa - 1, 0) << ((mantissa + 1).U - leadingOneFinder.io.out)
        }
      }
    }
    val reg_out_s = RegInit(0.U(bw.W))
    //val reg_out_s = Reg(UInt(bw.W))
    when(io.in_en){
      reg_out_s := new_s ## new_out_exp ## new_out_frac
      io.out_s := reg_out_s
    }.otherwise{
      io.out_s := reg_out_s
    }
    // combine all of the final results
  }

  // FP subtraction

  class FP_subber_v2(bw: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })
    // the subtraction is just a special case of the adder
    val FP_adder = Module(new FP_adder_v2(bw))
    val adjusted_in_b = WireInit(0.U(bw.W))
    // the second input needs to have its sign bit inverted for using the adder for subtraction
    adjusted_in_b := (~io.in_b(bw - 1)) ## io.in_b(bw - 2, 0)
    FP_adder.io.in_en := io.in_en
    FP_adder.io.in_a := io.in_a
    FP_adder.io.in_b := adjusted_in_b
    io.out_s := FP_adder.io.out_s
  }


  class FP_multiplier_v2(bw: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
    })
    var exponent = 0
    var mantissa = 0

    if (bw == 16) {
      exponent = 5
      mantissa = 10
    } else if (bw == 32) {
      exponent = 8
      mantissa = 23
    } else if (bw == 64) {
      exponent = 11
      mantissa = 52
    } else if (bw == 128) {
      exponent = 15
      mantissa = 112
    }
    // get the sign bit of the two inptus
    val s = Wire(Vec(2, UInt(1.W)))
    s(0) := io.in_a(bw - 1)
    s(1) := io.in_b(bw - 1)

    // get the exponents of the two inputs
    val exp = Wire(Vec(2, UInt(exponent.W)))
    when(io.in_a(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) { // there is a maximum number according to IEEE 754 standards
      exp(0) := BigInt(2).pow(exponent).U - 2.U
    }.otherwise {
      exp(0) := io.in_a(bw - 2, mantissa)
    }
    when(io.in_b(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) {
      exp(1) := BigInt(2).pow(exponent).U - 2.U
    }.otherwise {
      exp(1) := io.in_b(bw - 2, mantissa)
    }

    val exp_check = Wire(Vec(2, UInt(mantissa.W)))
    exp_check(0) := io.in_a(bw - 2, mantissa)
    exp_check(1) := io.in_b(bw - 2, mantissa)

    val cond_holder = Wire(UInt(mantissa.W))
    cond_holder := exp_check(0) + 1.U + ~((BigInt(2).pow(exponent - 1) - 1).U - exp_check(1))

    // get the mantissa parts of the two inputs
    val frac = Wire(Vec(2, UInt(mantissa.W)))
    frac(0) := io.in_a(mantissa - 1, 0)
    frac(1) := io.in_b(mantissa - 1, 0)

    // 1.0 + mantissa part of the two numbers
    val new_frac = Wire(Vec(2, UInt((mantissa + 1).W)))
    new_frac(0) := 1.U ## frac(0)
    new_frac(1) := 1.U ## frac(1)

    // use normal multiplier for multiplying the fractional/mantissa parts of the two inputs

    val multiplier = Module(new multiplier(mantissa + 1))
    multiplier.io.in_a := new_frac(0)
    multiplier.io.in_b := new_frac(1)

    // subtract exponent value of the second input from the bias value
    val subber = Module(new full_subber(exponent))
    subber.io.in_a := (BigInt(2).pow(exponent - 1) - 1).U // the bias
    subber.io.in_b := exp(1) // the second input
    subber.io.in_c := 0.U

    // will take twoscomplement of subtraction result
    val complementN = Module(new twoscomplement(exponent))
    complementN.io.in := subber.io.out_s

    // will add the twoscomplement result to the first input exponent
    val adderN = Module(new full_adder(exponent))
    adderN.io.in_a := exp(0) // the first input
    adderN.io.in_b := complementN.io.out // the twoscomplement result
    adderN.io.in_c := 0.U

    // equation for determining the sign of the multiplication result
    val new_s = Wire(UInt(1.W))
    new_s := s(0) ^ s(1)


    val new_exp = Wire(UInt(exponent.W)) // holds the final exponent value
    new_exp := 0.U
    val new_mant = Wire(UInt(mantissa.W)) //holds the final mantissa value
    new_mant := 0.U
    val cond_check = Wire(UInt((mantissa).W))
    cond_check := 0.U
    // if the msb of the mantissa multiplication result is 1, then we must add one to the exponent
    when(multiplier.io.out_s(((mantissa + 1) * 2) - 1) === 1.U) {
      new_exp := adderN.io.out_s + 1.U
      cond_check := adderN.io.out_s + 1.U
      cond_holder := exp_check(0) + 2.U + ~((BigInt(2).pow(exponent - 1) - 1).U - exp_check(1))
      new_mant := multiplier.io.out_s(((mantissa + 1) * 2) - 2, mantissa + 1)
    }.otherwise { // otherwise, leave everything as it is and just store the results
      new_exp := adderN.io.out_s
      cond_check := adderN.io.out_s
      cond_holder := exp_check(0) + 1.U + ~((BigInt(2).pow(exponent - 1) - 1).U - exp_check(1))
      new_mant := multiplier.io.out_s(((mantissa + 1) * 2) - 2, mantissa + 1) << (1).U
    }
    val reg_out_s = RegInit(0.U(bw.W))
    //val reg_out_s = Reg(UInt(bw.W))
    when(io.in_en) {
      when(BigInt(2).pow(exponent).U - 2.U >= 1.U + ~(cond_holder)) {
        new_exp := 1.U(exponent.W)
        new_mant := BigInt(2).pow(mantissa - 1).U(mantissa.W)
        reg_out_s := new_s ## new_exp ## new_mant
      }
        .elsewhen(cond_holder > BigInt(2).pow(exponent).U - 2.U) {
          new_exp := BigInt(2).pow(exponent).U - 2.U
          new_mant := BigInt(2).pow(mantissa).U - 1.U
          reg_out_s := new_s ## new_exp ## new_mant
        }
      when(exp(0) === 0.U || exp(1) === 0.U) { // if multiplication by 0, output is 0
        reg_out_s := 0.U
      }.otherwise { // otherwise just output the computed value
        reg_out_s := new_s ## new_exp ## new_mant
      }
      io.out_s := reg_out_s
    }.otherwise{
      io.out_s := reg_out_s
    }
  }

}
