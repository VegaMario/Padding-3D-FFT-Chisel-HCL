package ChiselFFT
import chisel3._
import chisel3.util._
import ComplexModules.FPComplex._
import SWFFT._
import SWFFT.FFT._
import IEEEConversions.FPConvert._
import Chisel.{MuxLookup, log2Ceil, resetToBool}
import ComplexModules.FPComplex
import SWFFT.ComplexNumbers.cmplx
import FFTSRDesigns._
import FFTMRDesigns._
import PermutationDesigns._
import DFTDesigns._

import scala.collection.mutable

object TwidFactorDesigns {
  class TwiddleFactorROM_forStreaming(N: Int, r: Int, w: Int, stage: Int, bw: Int, inv: Boolean) extends Module{
    val io = IO(new Bundle() {
      val in_addr = Input(UInt(log2Ceil(N).W))
      val out_data = Output(Vec(w, new ComplexNum(bw)))
    })
    val Twid_Constants = if(inv){Permutations.T2_inv(N, r)(stage)}else{Permutations.T2(N, r)(stage)}
    val Twid_map_2D = for(i <- 0 until N/w)yield{
      val tm = for(j <- 0 until w) yield{
        Twid_Constants(i*w+j)
      }
      tm
    }
    val initial_vec_map_re = for(i <- 0 until N/w) yield{
      VecInit(Twid_map_2D(i).map(x=>convert_string_to_IEEE_754(x.re.toString, bw).U(bw.W)))
    }
    val initial_vec_map_im = for(i <- 0 until N/w) yield{
      VecInit(Twid_map_2D(i).map(x=>convert_string_to_IEEE_754(x.im.toString, bw).U(bw.W)))
    }
    val vec_map_re = VecInit(initial_vec_map_re)
    val vec_map_im = VecInit(initial_vec_map_im)
    for(i <- 0 until w){
      io.out_data(i).Re := vec_map_re(io.in_addr)(i)
      io.out_data(i).Im := vec_map_im(io.in_addr)(i)
    }
  }

  class TwiddleFactorROM_mr_forStreaming(N: Int, s: Int, r: Int, w: Int, bw: Int, inv: Boolean) extends Module{
    val io = IO(new Bundle() {
      val in_addr = Input(UInt(log2Ceil(N).W))
      val out_data = Output(Vec(w, new ComplexNum(bw)))
    })
    val Twid_Constants = if(inv){Permutations.T2_rs_inv(N,s,r)}else{Permutations.T2_rs(N,s,r)}
    val Twid_map_2D = for(i <- 0 until N/w)yield{
      val tm = for(j <- 0 until w) yield{
        Twid_Constants(i*w+j)
      }
      tm
    }
    val initial_vec_map_re = for(i <- 0 until N/w) yield{
      VecInit(Twid_map_2D(i).map(x=>convert_string_to_IEEE_754(x.re.toString, bw).U(bw.W)))
    }
    val initial_vec_map_im = for(i <- 0 until N/w) yield{
      VecInit(Twid_map_2D(i).map(x=>convert_string_to_IEEE_754(x.im.toString, bw).U(bw.W)))
    }
    val vec_map_re = VecInit(initial_vec_map_re)
    val vec_map_im = VecInit(initial_vec_map_im)
    for(i <- 0 until w){
      io.out_data(i).Re := vec_map_re(io.in_addr)(i)
      io.out_data(i).Im := vec_map_im(io.in_addr)(i)
    }
  }

  class TwiddleFactorsStreamed_v2(N: Int, r: Int, w: Int, stage: Int, bw: Int, inv: Boolean) extends Module {
    val io = IO(new Bundle() {
      val in_en_main = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = if(inv){Permutations.T2_inv(N, r)(stage)}else{Permutations.T2(N, r)(stage)}
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_forStreaming(N, r, w, stage, bw, inv)).io // initialize ROM with twiddle factors
    for (i <- 0 until N) {
      if (DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001) {
        mults += 1
      }
    }
    if (mults > 0) {
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N / w).W))
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          is_enabled := true.B
        }.otherwise {
          is_enabled := false.B
        }
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new FPComplexMult_v2(bw)).io
        instance
      }
      for(i <- 0 until w){
        multipliers(i).in_en := io.in_en_main
      }
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          when(cnt === ((N / w) - 1).U) {
            cnt := 0.U
          }.otherwise {
            cnt := cnt + 1.U
          }
          for (i <- 0 until w) { // multiply module inputs with the twiddle factor outputs
            multipliers(i).in_a := io.in(i)
            multipliers(i).in_b := Twid_ROM.out_data(i)
          }
        }.otherwise {
          for (i <- 0 until w) {
            multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
          }
//          cnt := 0.U
        }
      }.otherwise{ // don't incremenet the counters
        when(io.in_en.asUInt.orR){
          for (i <- 0 until w) {
            multipliers(i).in_a := io.in(i)
            multipliers(i).in_b := Twid_ROM.out_data(i)
          }
        }.otherwise{
          for (i <- 0 until w) {
            multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
          }
        }
      }
      for (i <- 0 until w) {
        io.out(i) := multipliers(i).out_s
      }
      Twid_ROM.in_addr := cnt
    } else { // no multipliers are required
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N / w).W))
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          is_enabled := true.B
        }.otherwise {
          is_enabled := false.B
        }
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new ComplexNum_AdjustOrder_v2(bw)).io
        instance
      }
      for(i <- 0 until w){
        multipliers(i).in_en := io.in_en_main
      }
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          when(cnt === ((N / w) - 1).U) {
            cnt := 0.U
          }.otherwise {
            cnt := cnt + 1.U
          }
          for (i <- 0 until w) {
            multipliers(i).in := io.in(i)
            multipliers(i).in_adj := 0.U
            when(Twid_ROM.out_data(i).Re(bw - 2, 0) === convert_string_to_IEEE_754("1.0", bw).U) {
              multipliers(i).is_flip := false.B
              when(Twid_ROM.out_data(i).Re(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }.otherwise { // might imply that imaginary part = 1 while real part = 0
              multipliers(i).is_flip := true.B // therefore real and im parts switch
              when(Twid_ROM.out_data(i).Im(bw - 1) === 1.U) { // is negative?
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }
          }
        }.otherwise {
//          cnt := 0.U
          for (i <- 0 until w) {
            multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_adj := 0.U
            multipliers(i).is_flip := false.B
            multipliers(i).is_neg := false.B
          }
        }
      }.otherwise{ // otherwise counters are disabled
        when(io.in_en.asUInt.orR){
          for (i <- 0 until w) {
            multipliers(i).in := io.in(i)
            multipliers(i).in_adj := 0.U
            when(Twid_ROM.out_data(i).Re(bw - 2, 0) === convert_string_to_IEEE_754("1.0", bw).U) {
              multipliers(i).is_flip := false.B
              when(Twid_ROM.out_data(i).Re(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }.otherwise {
              multipliers(i).is_flip := true.B
              when(Twid_ROM.out_data(i).Im(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }
          }
        }.otherwise{
          for (i <- 0 until w) {
            multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_adj := 0.U
            multipliers(i).is_flip := false.B
            multipliers(i).is_neg := false.B
          }
        }
      }
      val result_regs = RegInit(VecInit.fill(2)(VecInit.fill(w)(0.U.asTypeOf(new ComplexNum(bw)))))
      when(io.in_en_main) {
        for (j <- 0 until 2) {
          if (j == 0) {
            for (i <- 0 until w) {
              result_regs(0)(i) := multipliers(i).out
            }
          } else {
            result_regs(j) := result_regs(j - 1)
          }
        }
      }
      io.out := result_regs(1)
      Twid_ROM.in_addr := cnt
    }
  }

  class TwiddleFactorsStreamed_mr_v2(N: Int, s: Int, r: Int, w: Int, bw: Int, delay: Int, inv: Boolean) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en_main = Input(Bool())
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = if(inv){Permutations.T2_rs_inv(N, s, r)}else{Permutations.T2_rs(N, s, r)}
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_mr_forStreaming(N, s, r, w, bw, inv)).io
    for (i <- 0 until N) {
      if (DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001) {
        mults += 1
      }
    }
    if (mults > 0) {
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N / w).W))
      val cnt2 = RegInit(0.U(log2Ceil(delay).W))
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          is_enabled := true.B
        }.otherwise {
          is_enabled := false.B
        }
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new FPComplexMult_v2(bw)).io
        instance
      }
      for(i <- 0 until w){
        multipliers(i).in_en := io.in_en_main
      }
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          when(cnt2 === (delay - 1).U) { // dont reset values until cnt2 has cycled delay times
            cnt2 := 0.U
            cnt := 0.U
          }.elsewhen(cnt === ((N / w) - 1).U && cnt2 =/= (delay - 1).U) {
            cnt := cnt
            cnt2 := cnt2 + 1.U
          }.otherwise {
            cnt := cnt + 1.U
            cnt2 := cnt2 + 1.U
          }
          for (i <- 0 until w) { // multiply inputs with twiddle factors
            multipliers(i).in_a := io.in(i)
            multipliers(i).in_b := Twid_ROM.out_data(i)
          }
        }.otherwise {
          for (i <- 0 until w) {
            multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
          }
//          cnt := 0.U
//          cnt2 := 0.U
        }
      }.otherwise{
        when(io.in_en.asUInt.orR){
          for (i <- 0 until w) {
            multipliers(i).in_a := io.in(i)
            multipliers(i).in_b := Twid_ROM.out_data(i)
          }
        }.otherwise{
          for (i <- 0 until w) {
            multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
          }
        }
      }
      for (i <- 0 until w) {
        io.out(i) := multipliers(i).out_s
      }
      Twid_ROM.in_addr := cnt
    } else { // otherwise no real multipliers are used
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N / w).W))
      val cnt2 = RegInit(0.U(log2Ceil(delay).W))
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          is_enabled := true.B
        }.otherwise {
          is_enabled := false.B
        }
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new ComplexNum_AdjustOrder_v2(bw)).io
        instance
      }
      for (i <- 0 until w) {
        multipliers(i).in_en := io.in_en_main
      }
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          when(cnt2 === (delay - 1).U) {
            cnt2 := 0.U
            cnt := 0.U
          }.elsewhen(cnt === ((N / w) - 1).U && cnt2 =/= (delay - 1).U) {
            cnt := cnt
            cnt2 := cnt2 + 1.U
          }.otherwise {
            cnt := cnt + 1.U
            cnt2 := cnt2 + 1.U
          }
          for (i <- 0 until w) {
            multipliers(i).in := io.in(i)
            multipliers(i).in_adj := 0.U
            when(Twid_ROM.out_data(i).Re(bw - 2, 0) === convert_string_to_IEEE_754("1.0", bw).U) {
              multipliers(i).is_flip := false.B
              when(Twid_ROM.out_data(i).Re(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }.otherwise {
              multipliers(i).is_flip := true.B
              when(Twid_ROM.out_data(i).Im(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }
          }
        }.otherwise {
          //          cnt := 0.U
          //          cnt2 := 0.U
          for (i <- 0 until w) {
            multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_adj := 0.U
            multipliers(i).is_flip := false.B
            multipliers(i).is_neg := false.B
          }
        }
      }.otherwise {
        when(io.in_en.asUInt.orR) {
          for (i <- 0 until w) {
            multipliers(i).in := io.in(i)
            multipliers(i).in_adj := 0.U
            when(Twid_ROM.out_data(i).Re(bw - 2, 0) === convert_string_to_IEEE_754("1.0", bw).U) {
              multipliers(i).is_flip := false.B
              when(Twid_ROM.out_data(i).Re(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }.otherwise {
              multipliers(i).is_flip := true.B
              when(Twid_ROM.out_data(i).Im(bw - 1) === 1.U) {
                multipliers(i).is_neg := true.B
              }.otherwise {
                multipliers(i).is_neg := false.B
              }
            }
          }
        }.otherwise {
          for (i <- 0 until w) {
            multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_adj := 0.U
            multipliers(i).is_flip := false.B
            multipliers(i).is_neg := false.B
          }
        }
      }
      val result_regs = RegInit(VecInit.fill(2)(VecInit.fill(w)(0.U.asTypeOf(new ComplexNum(bw)))))
      when(io.in_en_main) {
        for (j <- 0 until 2) {
          if (j == 0) {
            for (i <- 0 until w) {
              result_regs(0)(i) := multipliers(i).out
            }
          } else {
            result_regs(j) := result_regs(j - 1)
          }
        }
      }
      io.out := result_regs(1)
      Twid_ROM.in_addr := cnt
    }
  }

  class TwiddleFactors_v2(N: Int, r: Int, w: Int, stage: Int, bw: Int, inv: Boolean) extends Module {
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
//    for (i <- 0 until TotalStages - 1) {
//      for (j <- 0 until N) {
//        if (Permutations.T2(N, r)(i)(j).re == Permutations.T2(N, r)(i)(j).im) {
//          for (k <- i + 1 until TotalStages) {
//            if (Permutations.T2(N, r)(k)(j).re == Permutations.T2(N, r)(k)(j).im) {
//            }
//          }
//        }
//      }
//    }
    val DFTr_Constants = if(inv){Permutations.T2_inv(N, r)(stage)}else{Permutations.T2(N, r)(stage)}
    var mults = 0
    for (i <- 0 until N) {
      if (DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001) { // if both real part and imaginary parts are non-zero
        mults += 1
      }
    }
    val addregs = if (mults > 0) {
      true
    } else {
      false
    } // it seems not too hard to optimize, but I'll come back to this since it is simpler than mixed-radix streaming
    val multipliers = for (i <- 0 until N) yield {
      // if the multiplier count can be reduced further, the FPComplexMult_reducable module should be able to identify this
      val instance = Module(new FPComplexMult_reducable_forSymmetric_v2(bw, DFTr_Constants(i).re, DFTr_Constants(i).im, addregs, false)).io
      instance
    }
    for (i <- 0 until N) {
      multipliers(i).in_en := io.in_en
      multipliers(i).in_a := io.in(i)
      multipliers(i).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(i).re.toString, bw).U
      multipliers(i).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(i).im.toString, bw).U
      io.out(i) := multipliers(i).out_s
    }
  }
}
