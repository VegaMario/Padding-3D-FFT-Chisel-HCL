package FloatingPointDesigns
import chisel3._
import chisel3.util._

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

  class FP_adder_v2_multistage(bw: Int) extends Module {
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
    when(io.in_a(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) { // this ensures that the max number does not go out of range
      exp(0) := BigInt(2).pow(exponent).U - 2.U // if the number is larger then we limit it
    }.otherwise {
      exp(0) := io.in_a(bw - 2, mantissa) // otherwise we just keep the same value
    }
    when(io.in_b(bw - 2, mantissa) > BigInt(2).pow(exponent).U - 2.U) { //same case applies to the second input
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

    // we will place these initial results in registers
    val sign_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U)))
    val exp_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U(exponent.W))))
    val wfrac_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U((mantissa + 1).W))))
    val frac_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U((mantissa).W))))

    when(io.in_en) {
      sign_pipe(0)(0) := sign(0)
      sign_pipe(0)(1) := sign(1)
      exp_pipe(0)(0) := exp(0)
      exp_pipe(0)(1) := exp(1)
      wfrac_pipe(0)(0) := whole_frac(0)
      wfrac_pipe(0)(1) := whole_frac(1)
      frac_pipe(0)(0) := frac(0)
      frac_pipe(0)(1) := frac(1)
    }


    val io_ina_pipe = RegInit(VecInit.fill(6)(0.U(bw.W)))
    val io_inb_pipe = RegInit(VecInit.fill(6)(0.U(bw.W)))
    when(io.in_en) {
      io_ina_pipe(0) := io.in_a
      io_inb_pipe(0) := io.in_b
    }


    // end of parallel input // 1 cycle



    // subtracts the exponent of second input from the first input // the very first step is to subtract the exponent parts
    val subber = Module(new full_subber(exponent))
    subber.io.in_a := exp_pipe(0)(0)
    subber.io.in_b := exp_pipe(0)(1)
    subber.io.in_c := 0.U


    val subber_reg_out_s_pipe = RegInit(VecInit.fill(5)(0.U(exponent.W)))
    val subber_reg_out_c_pipe = RegInit(VecInit.fill(5)(0.U(1.W)))
    when(io.in_en) {
      subber_reg_out_s_pipe(0) := subber.io.out_s
      subber_reg_out_c_pipe(0) := subber.io.out_c

      sign_pipe(1)(0) := sign_pipe(0)(0)
      sign_pipe(1)(1) := sign_pipe(0)(1)
      exp_pipe(1)(0) := exp_pipe(0)(0)
      exp_pipe(1)(1) := exp_pipe(0)(1)
      wfrac_pipe(1)(0) := wfrac_pipe(0)(0)
      wfrac_pipe(1)(1) := wfrac_pipe(0)(1)
      frac_pipe(1)(0) := frac_pipe(0)(0)
      frac_pipe(1)(1) := frac_pipe(0)(1)

      io_ina_pipe(1) := io_ina_pipe(0)
      io_inb_pipe(1) := io_inb_pipe(0)
    }

    // end of step // 2 cycle

    val out_s = Wire(UInt(1.W)) // sign of the larger input
    val out_frac = Wire(UInt(mantissa.W)) // mantissa/fractional part of larger input
    val out_exp = Wire(UInt(exponent.W)) // exponent of larger inpui
    val sub_exp = Wire(UInt(exponent.W)) // temporarily holds the result of exp(0) - exp(1) (difference in exponents)


    // just set to 0 initially
    out_exp := 0.U
    sub_exp := 0.U
    out_frac := 0.U
    out_s := 0.U


    // in case the exponent difference is negative, take the twos complement of subtraction result
    val complement = Module(new twoscomplement(exponent))
    complement.io.in := subber_reg_out_s_pipe(0)

    // add the fractional/mantissa parts of the ieee numbers together
    val adder = Module(new full_adder(mantissa + 1))
    adder.io.in_c := 0.U

    // these instantiations of twoscomplement are when there are negative number inputs
    val complementN_0 = Module(new twoscomplement(mantissa + 1))

    val complementN_1 = Module(new twoscomplement(mantissa + 1))


    // before we add the mantissa parts together, we need to align the exponents by shifting by the exponent difference
    val shifter = Module(new shifter(mantissa + 1))

    // if the exponent difference resulted in a borrow, then the second input has a larger exponent than the first
    when(subber_reg_out_c_pipe(0) === 1.U) {
      out_exp := exp_pipe(1)(1)
      sub_exp := complement.io.out // the exponent difference is in the negative form, so take the twos complement
      out_s := sign_pipe(1)(1)
      out_frac := frac_pipe(1)(1)
      shifter.io.in_a := wfrac_pipe(1)(0) // shift the smaller exponent input to the right for aligning
      shifter.io.in_b := complement.io.out
      shifter.io.in_c := 1.U
      complementN_0.io.in := shifter.io.out_s
      complementN_1.io.in := wfrac_pipe(1)(1)
      when(sign_pipe(1)(0) === 1.U && sign_pipe(1)(1) === 0.U) {
        adder.io.in_a := complementN_0.io.out
      }.otherwise{
        adder.io.in_a := shifter.io.out_s
      }
      // if second input is negative, take twos complement, and compute mantissa addition
      when(sign_pipe(1)(1) === 1.U && sign_pipe(1)(0) === 0.U) {
        adder.io.in_b := complementN_1.io.out
      }.otherwise{
        adder.io.in_b := wfrac_pipe(1)(1)
      }
//      adder.io.in_a := shifter.io.out_s // now we can add the two aligned mantissa/fractional parts
//      adder.io.in_b := wfrac_pipe(1)(1)
    }.otherwise { // otherwise the first input has a larger exponent or the same as the second input
      out_exp := exp_pipe(1)(0)
      sub_exp := subber_reg_out_s_pipe(0) // the exponent difference is not negative, so just leave it alone
      out_s := sign_pipe(1)(0)
      out_frac := frac_pipe(1)(0)
      shifter.io.in_a := wfrac_pipe(1)(1) // shift the smaller exponent input to the right for aligning
      shifter.io.in_b := subber_reg_out_s_pipe(0)
      shifter.io.in_c := 1.U
      complementN_0.io.in := wfrac_pipe(1)(0)
      complementN_1.io.in := shifter.io.out_s
      // if first input is negative, take twos complement
      when(sign_pipe(1)(0) === 1.U && sign_pipe(1)(1) === 0.U) {
        adder.io.in_a := complementN_0.io.out
      }.otherwise{
        adder.io.in_a := wfrac_pipe(1)(0)
      }
      // if second input is negative, take twos complement, and compute mantissa addition
      when(sign_pipe(1)(1) === 1.U && sign_pipe(1)(0) === 0.U) {
        adder.io.in_b := complementN_1.io.out
      }.otherwise{
        adder.io.in_b := shifter.io.out_s
      }
//      adder.io.in_a := wfrac_pipe(1)(0)
//      adder.io.in_b := shifter.io.out_s // now we can dd the two aligned mantissa parts
    }

    val out_s_pipe = RegInit(VecInit.fill(4)(0.U))
    val out_frac_pipe = RegInit(VecInit.fill(4)(0.U(mantissa.W)))
    val out_exp_pipe = RegInit(VecInit.fill(4)(0.U(exponent.W)))
    val sub_exp_pipe = RegInit(VecInit.fill(4)(0.U(exponent.W)))

    val adder_out_s_pipe = RegInit(VecInit.fill(4)(0.U((mantissa+1).W)))
    val adder_out_c_pipe = RegInit(VecInit.fill(4)(0.U((1).W)))
    when(io.in_en) {
      adder_out_s_pipe(0) := adder.io.out_s
      adder_out_c_pipe(0) := adder.io.out_c

      out_exp_pipe(0) := out_exp
      sub_exp_pipe(0) := sub_exp
      out_s_pipe(0) := out_s
      out_frac_pipe(0) := out_frac

      sign_pipe(2)(0) := sign_pipe(1)(0)
      sign_pipe(2)(1) := sign_pipe(1)(1)
      exp_pipe(2)(0) := exp_pipe(1)(0)
      exp_pipe(2)(1) := exp_pipe(1)(1)
      wfrac_pipe(2)(0) := wfrac_pipe(1)(0)
      wfrac_pipe(2)(1) := wfrac_pipe(1)(1)
      frac_pipe(2)(0) := frac_pipe(1)(0)
      frac_pipe(2)(1) := frac_pipe(1)(1)

      subber_reg_out_s_pipe(1) := subber_reg_out_s_pipe(0)
      subber_reg_out_c_pipe(1) := subber_reg_out_c_pipe(0)


      io_ina_pipe(2) := io_ina_pipe(1)
      io_inb_pipe(2) := io_inb_pipe(1)
    }

    // end of step // 3 cycle



    val new_s = Wire(UInt(1.W)) // will hold the final sign result

    new_s := (~adder_out_c_pipe(0) & sign_pipe(2)(0)) | (sign_pipe(2)(0) & sign_pipe(2)(1)) | (~adder_out_c_pipe(0) & sign_pipe(2)(1)) // this binary equation gives the new sign of the sum


    // this twoscomplement is here to take the two complement of the mantissa sum in case the new sign of the sum is negative
    val complementN_2 = Module(new twoscomplement(mantissa + 1))
    complementN_2.io.in := adder_out_s_pipe(0)


    val D = Wire(UInt(1.W)) // will indicate in which direction the mantissa sum will have to be shifted to be correct. (D = 0 indicates current exponent needs to be increased, D= 1 indicates that the current exponent needs to be decreased)
    val E = Wire(UInt(1.W)) // will indicate if there is no need for adjusting the mantissa sum and we can leave it as it is.

    // equation for describing the conditions in which the mantissa sum will need to be shifted
    D := (~adder_out_c_pipe(0)) | (sign_pipe(2)(0) ^ sign_pipe(2)(1))

    // equation for the conditions in which the manstissa sum does not need to be shifted. This has higher priority than D
    E := (~adder_out_c_pipe(0) & ~adder_out_s_pipe(0)(mantissa)) | (~adder_out_c_pipe(0) & ~(sign_pipe(2)(0) ^ sign_pipe(2)(1))) | (adder_out_c_pipe(0) & adder_out_s_pipe(0)(mantissa) & (sign_pipe(2)(0) ^ sign_pipe(2)(1)))

    val adder_result = Wire(UInt((mantissa + 1).W))
    when(new_s === 1.U && sign_pipe(2)(0) =/= sign_pipe(2)(1)) { // if the new sign of the sum is negative, then take the twos complement of the mantissa sum
      adder_result := complementN_2.io.out
    }.otherwise{
      adder_result := adder_out_s_pipe(0)
    }

    val new_s_pipe = RegInit(VecInit.fill(4)(0.U((1).W)))
    val D_pipe = RegInit(VecInit.fill(3)(0.U((1).W)))
    val E_pipe = RegInit(VecInit.fill(3)(0.U((1).W)))
//    printf(p"D: ${D_pipe(0)}")
    when(io.in_en) {
      new_s_pipe(0) := new_s
      D_pipe(0) := D
      E_pipe(0) := E
    }
    val adder_result_pipe = RegInit(VecInit.fill(3)(0.U((mantissa+1).W)))
    when(io.in_en) {
      adder_result_pipe(0) := adder_result

      adder_out_s_pipe(1) := adder_out_s_pipe(0)
      adder_out_c_pipe(1) := adder_out_c_pipe(0)

      out_exp_pipe(1) := out_exp_pipe(0)
      sub_exp_pipe(1) := sub_exp_pipe(0)
      out_s_pipe(1) := out_s_pipe(0)
      out_frac_pipe(1) := out_frac_pipe(0)

      sign_pipe(3)(0) := sign_pipe(2)(0)
      sign_pipe(3)(1) := sign_pipe(2)(1)
      exp_pipe(3)(0) := exp_pipe(2)(0)
      exp_pipe(3)(1) := exp_pipe(2)(1)
      wfrac_pipe(3)(0) := wfrac_pipe(2)(0)
      wfrac_pipe(3)(1) := wfrac_pipe(2)(1)
      frac_pipe(3)(0) := frac_pipe(2)(0)
      frac_pipe(3)(1) := frac_pipe(2)(1)

      subber_reg_out_s_pipe(2) := subber_reg_out_s_pipe(1)
      subber_reg_out_c_pipe(2) := subber_reg_out_c_pipe(1)


      io_ina_pipe(3) := io_ina_pipe(2)
      io_inb_pipe(3) := io_inb_pipe(2)
    }

    // end of step // 4 cycle

    // module finds the most significant 1 in the mantissa sum. This is used when E = 0, and D = 1, indicating a left shift
    val leadingOneFinder = Module(new leadingOneDetector(mantissa + 1))
    leadingOneFinder.io.in := adder_result_pipe(0)

    val leadingOneFinder_pipe = RegInit(VecInit.fill(2)(0.U((log2Floor(bw) + 1).W)))
    when(io.in_en) {
      leadingOneFinder_pipe(0) := leadingOneFinder.io.out

      new_s_pipe(1) := new_s_pipe(0)
      D_pipe(1) := D_pipe(0)
      E_pipe(1) := E_pipe(0)

      adder_result_pipe(1) := adder_result_pipe(0)


      adder_out_s_pipe(2) := adder_out_s_pipe(1)
      adder_out_c_pipe(2) := adder_out_c_pipe(1)

      out_exp_pipe(2) := out_exp_pipe(1)
      sub_exp_pipe(2) := sub_exp_pipe(1)
      out_s_pipe(2) := out_s_pipe(1)
      out_frac_pipe(2) := out_frac_pipe(1)

      sign_pipe(4)(0) := sign_pipe(3)(0)
      sign_pipe(4)(1) := sign_pipe(3)(1)
      exp_pipe(4)(0) := exp_pipe(3)(0)
      exp_pipe(4)(1) := exp_pipe(3)(1)
      wfrac_pipe(4)(0) := wfrac_pipe(3)(0)
      wfrac_pipe(4)(1) := wfrac_pipe(3)(1)
      frac_pipe(4)(0) := frac_pipe(3)(0)
      frac_pipe(4)(1) := frac_pipe(3)(1)

      subber_reg_out_s_pipe(3) := subber_reg_out_s_pipe(2)
      subber_reg_out_c_pipe(3) := subber_reg_out_c_pipe(2)

      io_ina_pipe(4) := io_ina_pipe(3)
      io_inb_pipe(4) := io_inb_pipe(3)
    }


    // end of step // 5 cycle

    val subber2 = Module(new full_subber(exponent))
    subber2.io.in_a := out_exp_pipe(2)
    subber2.io.in_b := ((mantissa + 1).U - leadingOneFinder_pipe(0))
    subber2.io.in_c := 0.U

    val subber2_out_s_pipe = RegInit(VecInit.fill(1)(0.U(exponent.W)))
    val subber2_out_c_pipe = RegInit(VecInit.fill(1)(0.U(1.W)))
    when(io.in_en) {
      subber2_out_s_pipe(0) := subber2.io.out_s
      subber2_out_c_pipe(0) := subber2.io.out_c


      leadingOneFinder_pipe(1) := leadingOneFinder_pipe(0)

      new_s_pipe(2) := new_s_pipe(1)
      D_pipe(2) := D_pipe(1)
      E_pipe(2) := E_pipe(1)

      adder_result_pipe(2) := adder_result_pipe(1)


      adder_out_s_pipe(3) := adder_out_s_pipe(2)
      adder_out_c_pipe(3) := adder_out_c_pipe(2)

      out_exp_pipe(3) := out_exp_pipe(2)
      sub_exp_pipe(3) := sub_exp_pipe(2)
      out_s_pipe(3) := out_s_pipe(2)
      out_frac_pipe(3) := out_frac_pipe(2)

      sign_pipe(5)(0) := sign_pipe(4)(0)
      sign_pipe(5)(1) := sign_pipe(4)(1)
      exp_pipe(5)(0) := exp_pipe(4)(0)
      exp_pipe(5)(1) := exp_pipe(4)(1)
      wfrac_pipe(5)(0) := wfrac_pipe(4)(0)
      wfrac_pipe(5)(1) := wfrac_pipe(4)(1)
      frac_pipe(5)(0) := frac_pipe(4)(0)
      frac_pipe(5)(1) := frac_pipe(4)(1)

      subber_reg_out_s_pipe(4) := subber_reg_out_s_pipe(3)
      subber_reg_out_c_pipe(4) := subber_reg_out_c_pipe(3)

      io_ina_pipe(5) := io_ina_pipe(4)
      io_inb_pipe(5) := io_inb_pipe(4)
    }

    // end of step // 6 cycle

//    val new_out_frac = Wire(UInt(mantissa.W)) // will hold the final mantissa result
//    val new_out_exp = Wire(UInt(exponent.W)) // will hold the final exponent result

    val new_out_frac_pipe = RegInit(VecInit.fill(1)(0.U(mantissa.W)))
    val new_out_exp_pipe = RegInit(VecInit.fill(1)(0.U(exponent.W)))
    printf(p"out_exp: ${(leadingOneFinder.io.out)}")
    when(io.in_en) {
      when(io_ina_pipe(5)(bw - 2, 0) === 0.U && io_inb_pipe(5)(bw - 2, 0) === 0.U) {
        new_s_pipe(3) := 0.U
        new_out_exp_pipe(0) := 0.U
        new_out_frac_pipe(0) := 0.U
      }.elsewhen(sub_exp_pipe(3) >= mantissa.U) { // if the difference between the exponents is too large, larger than mantissa size.
        new_s_pipe(3) := out_s_pipe(3)
        new_out_frac_pipe(0) := out_frac_pipe(3)
        new_out_exp_pipe(0) := out_exp_pipe(3)
      }.elsewhen(E_pipe(2) === 1.U) { // if the exponent should stay the same size as the largest exponent
        new_s_pipe(3) := new_s_pipe(2)
        new_out_exp_pipe(0) := out_exp_pipe(3)
        new_out_frac_pipe(0) := adder_result_pipe(2)(mantissa - 1, 0)
      }.elsewhen(D_pipe(2) === 0.U) { // if exponent needs to be increased by 1
        when(out_exp_pipe(3) === BigInt(2).pow(exponent).U - 2.U) {
          new_s_pipe(3) := new_s_pipe(2)
          new_out_exp_pipe(0) := BigInt(2).pow(exponent).U - 2.U
          new_out_frac_pipe(0) := BigInt(2).pow(mantissa).U - 1.U
        }.otherwise {
          new_s_pipe(3) := new_s_pipe(2)
          new_out_exp_pipe(0) := out_exp_pipe(3) + 1.U
          new_out_frac_pipe(0) := adder_result_pipe(2)(mantissa, 1)
        }
      }.elsewhen(D_pipe(2) === 1.U) { // if exponent needs to be decreased by 1 or more
        when(leadingOneFinder_pipe(1) === 1.U && adder_result_pipe(2) === 0.U && ((1.U === (sign_pipe(5)(0) ^ sign_pipe(5)(1)) && io_ina_pipe(5)(bw - 2, 0) === io_inb_pipe(5)(bw - 2, 0)))) {
          new_s_pipe(3) := new_s_pipe(2)
          new_out_exp_pipe(0) := 0.U
          new_out_frac_pipe(0) := 0.U
        }.otherwise {
          when(subber2_out_c_pipe(0) === 1.U) {
            new_s_pipe(3) := new_s_pipe(2)
            new_out_exp_pipe(0) := 1.U(exponent.W)
            new_out_frac_pipe(0) := BigInt(2).pow(mantissa - 1).U(mantissa.W)
          }.otherwise {
            new_s_pipe(3) := new_s_pipe(2)
            new_out_exp_pipe(0) := subber2_out_s_pipe(0)
            new_out_frac_pipe(0) := adder_result_pipe(2)(mantissa - 1, 0) << ((mantissa + 1).U - leadingOneFinder_pipe(1))
          }
        }
      }
    }

    val reg_out_s = RegInit(0.U(bw.W))
    // end of step // 7 cycle

    //val reg_out_s = Reg(UInt(bw.W))
//    printf(p"new_s_pipe: ${new_out_frac_pipe(0)}")
    when(io.in_en){
      reg_out_s := new_s_pipe(3) ## new_out_exp_pipe(0) ## new_out_frac_pipe(0)
      io.out_s := reg_out_s
    }.otherwise{
      io.out_s := reg_out_s
    }
    // combine all of the final results
  } // an 8 cycle adder

  class FP_subber_v2_multistage(bw:Int) extends Module{
    val io = IO(new Bundle{
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_en = Input(Bool())
      val out_s = Output(UInt(bw.W))
    })
    val adder = Module(new FP_adder_v2_multistage(bw)).io
    adder.in_a := io.in_a
    adder.in_b := ~io.in_b(bw-1) ## io.in_b(bw-2,0)
    adder.in_en := io.in_en
    io.out_s := adder.out_s
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

  // now I will attempt to make a multi cycle  multiplier
  class FP_multuplier_v2_multistage(bw:Int) extends Module{
    val io = IO(new Bundle{
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val in_en = Input(Bool())
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

    val s_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U(1.W))))
    val exp_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U(exponent.W))))
    val exp_check_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U(mantissa.W))))
    val cond_holder_pipe = RegInit(VecInit.fill(6)(0.U(mantissa.W)))
    val frac_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U(mantissa.W))))
    val new_frac_pipe = RegInit(VecInit.fill(6)(VecInit.fill(2)(0.U((mantissa + 1).W))))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(0)(i) := s(i)
        exp_pipe(0)(i) := exp(i)
        exp_check_pipe(0)(i) := exp_check(i)
        frac_pipe(0)(i) := frac(i)
        new_frac_pipe(0)(i) := new_frac(i)
        cond_holder_pipe(0) := cond_holder
      }
    }

    // end of step // 1 cycle

    // use normal multiplier for multiplying the fractional/mantissa parts of the two inputs

    val multiplier = Module(new multiplier(mantissa + 1))
    multiplier.io.in_a := new_frac_pipe(0)(0)
    multiplier.io.in_b := new_frac_pipe(0)(1)


    val multiplier_pipe = RegInit(VecInit.fill(5)(0.U(((mantissa+1)*2).W)))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(1)(i) := s_pipe(0)(i)
        exp_pipe(1)(i) := exp_pipe(0)(i)
        exp_check_pipe(1)(i) := exp_check_pipe(0)(i)
        frac_pipe(1)(i) := frac_pipe(0)(i)
        new_frac_pipe(1)(i) := new_frac_pipe(0)(i)
        cond_holder_pipe(1) := cond_holder_pipe(0)
        multiplier_pipe(0) := multiplier.io.out_s
      }
    }



    // end of step // 2 cycle




    // subtract exponent value of the second input from the bias value
    val subber = Module(new full_subber(exponent))
    subber.io.in_a := (BigInt(2).pow(exponent - 1) - 1).U // the bias
    subber.io.in_b := exp_pipe(1)(1) // the second input
    subber.io.in_c := 0.U


    val subber_out_s_pipe = RegInit(VecInit.fill(4)(0.U(exponent.W)))
    val subber_out_c_pipe = RegInit(VecInit.fill(4)(0.U(1.W)))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(2)(i) := s_pipe(1)(i)
        exp_pipe(2)(i) := exp_pipe(1)(i)
        exp_check_pipe(2)(i) := exp_check_pipe(1)(i)
        frac_pipe(2)(i) := frac_pipe(1)(i)
        new_frac_pipe(2)(i) := new_frac_pipe(1)(i)
        cond_holder_pipe(2) := cond_holder_pipe(1)
        multiplier_pipe(1) := multiplier_pipe(0)
        subber_out_s_pipe(0) := subber.io.out_s
        subber_out_c_pipe(0) := subber.io.out_c
      }
    }

    // end of step // 3 cycle


    // will take twoscomplement of subtraction result
    val complementN = Module(new twoscomplement(exponent))
    complementN.io.in := subber_out_s_pipe(0)

    val complementN_pipe = RegInit(VecInit.fill(3)(0.U(exponent.W)))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(3)(i) := s_pipe(2)(i)
        exp_pipe(3)(i) := exp_pipe(2)(i)
        exp_check_pipe(3)(i) := exp_check_pipe(2)(i)
        frac_pipe(3)(i) := frac_pipe(2)(i)
        new_frac_pipe(3)(i) := new_frac_pipe(2)(i)
        cond_holder_pipe(3) := cond_holder_pipe(2)
        multiplier_pipe(2) := multiplier_pipe(1)
        subber_out_s_pipe(1) := subber_out_s_pipe(0)
        subber_out_c_pipe(1) := subber_out_c_pipe(0)
        complementN_pipe(0) := complementN.io.out
      }
    }

    // end of step // 4 cycles

    // will add the twoscomplement result to the first input exponent
    val adderN = Module(new full_adder(exponent))
    adderN.io.in_a := exp_pipe(3)(0) // the first input
    adderN.io.in_b := complementN_pipe(0) // the twoscomplement result
    adderN.io.in_c := 0.U

    // equation for determining the sign of the multiplication result
    val new_s = Wire(UInt(1.W))
    new_s := s_pipe(3)(0) ^ s_pipe(3)(1)

    val adderN_out_s_pipe = RegInit(VecInit.fill(2)(0.U(exponent.W)))
    val adderN_out_c_pipe = RegInit(VecInit.fill(2)(0.U(1.W)))
    val new_s_pipe = RegInit(VecInit.fill(3)(0.U(1.W)))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(4)(i) := s_pipe(3)(i)
        exp_pipe(4)(i) := exp_pipe(3)(i)
        exp_check_pipe(4)(i) := exp_check_pipe(3)(i)
        frac_pipe(4)(i) := frac_pipe(3)(i)
        new_frac_pipe(4)(i) := new_frac_pipe(3)(i)
        cond_holder_pipe(4) := cond_holder_pipe(3)
        multiplier_pipe(3) := multiplier_pipe(2)
        subber_out_s_pipe(2) := subber_out_s_pipe(1)
        subber_out_c_pipe(2) := subber_out_c_pipe(1)
        complementN_pipe(1) := complementN_pipe(0)
        adderN_out_s_pipe(0) := adderN.io.out_s
        adderN_out_c_pipe(0) := adderN.io.out_c
        new_s_pipe(0) := new_s
      }
    }

    // end of step // 5 cycle

    val new_exp = Wire(UInt(exponent.W)) // holds the final exponent value
    new_exp := 0.U
    val new_mant = Wire(UInt(mantissa.W)) //holds the final mantissa value
    new_mant := 0.U
    val cond_check = Wire(UInt((mantissa).W))
    cond_check := 0.U

    // if the msb of the mantissa multiplication result is 1, then we must add one to the exponent
    when(multiplier_pipe(3)(((mantissa + 1) * 2) - 1) === 1.U) {
      new_exp := adderN_out_s_pipe(0) + 1.U
      cond_check := adderN_out_s_pipe(0) + 1.U
      cond_holder := exp_check_pipe(4)(0) + 2.U + ~((BigInt(2).pow(exponent - 1) - 1).U - exp_check_pipe(4)(1))
      new_mant := multiplier_pipe(3)(((mantissa + 1) * 2) - 2, mantissa + 1)
    }.otherwise { // otherwise, leave everything as it is and just store the results
      new_exp := adderN_out_s_pipe(0)
      cond_check := adderN_out_s_pipe(0)
      cond_holder := exp_check_pipe(4)(0) + 1.U + ~((BigInt(2).pow(exponent - 1) - 1).U - exp_check_pipe(4)(1))
      new_mant := multiplier_pipe(3)(((mantissa + 1) * 2) - 2, mantissa + 1) << (1).U
    }

    val new_exp_pipe = RegInit(VecInit.fill(2)(0.U(exponent.W)))
    val new_mant_pipe = RegInit(VecInit.fill(2)(0.U(mantissa.W)))
    val cond_check_pipe = RegInit(VecInit.fill(1)(0.U(mantissa.W)))

    when(io.in_en) {
      for (i <- 0 until 2) {
        s_pipe(5)(i) := s_pipe(4)(i)
        exp_pipe(5)(i) := exp_pipe(4)(i)
        exp_check_pipe(5)(i) := exp_check_pipe(4)(i)
        frac_pipe(5)(i) := frac_pipe(4)(i)
        new_frac_pipe(5)(i) := new_frac_pipe(4)(i)
        cond_holder_pipe(5) := cond_holder_pipe(4)
        multiplier_pipe(4) := multiplier_pipe(3)
        subber_out_s_pipe(3) := subber_out_s_pipe(2)
        subber_out_c_pipe(3) := subber_out_c_pipe(2)
        complementN_pipe(2) := complementN_pipe(1)
        adderN_out_s_pipe(1) := adderN_out_s_pipe(0)
        adderN_out_c_pipe(1) := adderN_out_c_pipe(0)
        new_s_pipe(1) := new_s_pipe(0)
        new_exp_pipe(0) := new_exp
        new_mant_pipe(0) := new_mant
        cond_check_pipe(0) := cond_check
      }
    }

    // end of step // 6 cycle

    val reg_out_s = RegInit(0.U(bw.W))
    //val reg_out_s = Reg(UInt(bw.W))

    val new_exp_w = Wire(UInt(exponent.W))
    val new_mant_w = Wire(UInt(mantissa.W))
    val new_s_w = Wire(UInt(1.W))
    new_exp_w := 0.U
    new_mant_w := 0.U
    new_s_w := 0.U

    when(io.in_en) {
      when(BigInt(2).pow(exponent).U - 2.U >= 1.U + ~(cond_holder_pipe(5))) {
        printf(p"Hey there deilia: ${1.U + ~(cond_holder_pipe(5))}")
        new_exp_w := 1.U(exponent.W)
        new_mant_w := BigInt(2).pow(mantissa - 1).U(mantissa.W)
        new_s_w := new_s_pipe(1)
      }.elsewhen(cond_holder_pipe(5) > BigInt(2).pow(exponent).U - 2.U) {
          new_exp_w := BigInt(2).pow(exponent).U - 2.U
          new_mant_w := BigInt(2).pow(mantissa).U - 1.U
          new_s_w := new_s_pipe(1)
        }

      when(exp_pipe(5)(0) === 0.U || exp_pipe(5)(1) === 0.U) { // if multiplication by 0, output is 0
        new_exp_w := 0.U
        new_mant_w := 0.U
        new_s_w := 0.U
      }.otherwise { // otherwise just output the computed value
        new_exp_w := new_exp_pipe(0)
        new_mant_w := new_mant_pipe(0)
        new_s_w := new_s_pipe(1)
      }
      new_exp_pipe(1) := new_exp_w
      new_mant_pipe(1) := new_mant_w
      new_s_pipe(2) := new_s_w
    }

    //  end of step // 7 cycles
    when(io.in_en) {
      reg_out_s := new_s_pipe(2) ## new_exp_pipe(1) ## new_mant_pipe(1)
    }
    // end of step // 8 cycles
    io.out_s := reg_out_s
  }

}
