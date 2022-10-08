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
  class TwiddleFactorROM_forStreaming(N: Int, r: Int, w: Int, stage: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_addr = Input(UInt(log2Ceil(N).W))
      val out_data = Output(Vec(w, new ComplexNum(bw)))
    })
    val Twid_Constants = Permutations.T2(N,r)(stage)
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

  class TwiddleFactorROM_mr_forStreaming(N: Int, s: Int, r: Int, w: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_addr = Input(UInt(log2Ceil(N).W))
      val out_data = Output(Vec(w, new ComplexNum(bw)))
    })
    val Twid_Constants = Permutations.T2_rs(N,s,r)
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

  class TwiddleFactorsStreamed(N: Int, r:Int, w: Int, stage:Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = Permutations.T2(N,r)(stage)
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_forStreaming(N,r,w,stage,bw)).io
    for(i <- 0 until N){
      if(DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001){
        mults += 1
      }
    }
    if(mults > 0) {
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      when(io.in_en.asUInt.orR){
        is_enabled := true.B
      }.otherwise{
        is_enabled := false.B
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new FPComplexMult(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt === ((N/w)-1).U){
          cnt := 0.U
        }.otherwise{
          cnt := cnt + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in_a := io.in(i)
          multipliers(i).in_b := Twid_ROM.out_data(i)
        }
      }.otherwise{
        for(i <- 0 until w){
          multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
        }
        cnt := 0.U
      }
      for(i <- 0 until w){
        io.out(i) := multipliers(i).out_s
      }
      Twid_ROM.in_addr := cnt
    }else{
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      when(io.in_en.asUInt.orR){
        is_enabled := true.B
      }.otherwise{
        is_enabled := false.B
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new ComplexNum_AdjustOrder(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt === ((N/w)-1).U){
          cnt := 0.U
        }.otherwise{
          cnt := cnt + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in := io.in(i)
          multipliers(i).in_adj := 0.U
          when(Twid_ROM.out_data(i).Re(bw-2,0) === convert_string_to_IEEE_754("1.0", bw).U){
            multipliers(i).is_flip := false.B
            when(Twid_ROM.out_data(i).Re(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }.otherwise{
            multipliers(i).is_flip := true.B
            when(Twid_ROM.out_data(i).Im(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }
        }
      }.otherwise{
        cnt := 0.U
        for(i <- 0 until w){
          multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_adj := 0.U
          multipliers(i).is_flip := false.B
          multipliers(i).is_neg := false.B
        }
      }
      val result_regs = RegInit(VecInit.fill(2)(VecInit.fill(w)(0.U.asTypeOf(new ComplexNum(bw)))))
      for(j <- 0 until 2) {
        if(j == 0) {
          for (i <- 0 until w) {
            result_regs(0)(i) := multipliers(i).out
          }
        }else{
          result_regs(j) := result_regs(j-1)
        }
      }
      io.out := result_regs(1)
      Twid_ROM.in_addr := cnt
    }
  }

  class TwiddleFactorsStreamed_v2(N: Int, r: Int, w: Int, stage: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in_en_main = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = Permutations.T2(N, r)(stage)
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_forStreaming(N, r, w, stage, bw)).io
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
          for (i <- 0 until w) {
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
    } else {
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
          for (i <- 0 until w) {
            multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
            multipliers(i).in_adj := 0.U
            multipliers(i).is_flip := false.B
            multipliers(i).is_neg := false.B
          }
        }
      }.otherwise{
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

  class TwiddleFactorsStreamed_mr(N: Int, s:Int, r:Int, w: Int, bw: Int, delay: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = Permutations.T2_rs(N,s,r)
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_mr_forStreaming(N,s,r,w,bw)).io
    for(i <- 0 until N){
      if(DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001){
        mults += 1
      }
    }
    if(mults > 0) {
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      val cnt2 = RegInit(0.U(log2Ceil(delay).W))
      when(io.in_en.asUInt.orR){
        is_enabled := true.B
      }.otherwise{
        is_enabled := false.B
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new FPComplexMult(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt2 === (delay - 1).U){
          cnt2 := 0.U
          cnt := 0.U
        }.elsewhen(cnt === ((N/w)-1).U && cnt2 =/= (delay - 1).U){
          cnt := cnt
          cnt2 := cnt2 + 1.U
        }.otherwise{
          cnt := cnt + 1.U
          cnt2 := cnt2 + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in_a := io.in(i)
          multipliers(i).in_b := Twid_ROM.out_data(i)
        }
      }.otherwise{
        for(i <- 0 until w){
          multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
        }
        cnt := 0.U
        cnt2 := 0.U
      }
      for(i <- 0 until w){
        io.out(i) := multipliers(i).out_s
      }
      Twid_ROM.in_addr := cnt
    }else{
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      val cnt2 = RegInit(0.U(log2Ceil(delay).W))
      when(io.in_en.asUInt.orR){
        is_enabled := true.B
      }.otherwise{
        is_enabled := false.B
      }
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new ComplexNum_AdjustOrder(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt2 === (delay - 1).U){
          cnt2 := 0.U
          cnt := 0.U
        }.elsewhen(cnt === ((N/w)-1).U && cnt2 =/= (delay - 1).U){
          cnt := cnt
          cnt2 := cnt2 + 1.U
        }.otherwise{
          cnt := cnt + 1.U
          cnt2 := cnt2 + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in := io.in(i)
          multipliers(i).in_adj := 0.U
          when(Twid_ROM.out_data(i).Re(bw-2,0) === convert_string_to_IEEE_754("1.0", bw).U){
            multipliers(i).is_flip := false.B
            when(Twid_ROM.out_data(i).Re(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }.otherwise{
            multipliers(i).is_flip := true.B
            when(Twid_ROM.out_data(i).Im(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }
        }
      }.otherwise{
        cnt := 0.U
        cnt2 := 0.U
        for(i <- 0 until w){
          multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_adj := 0.U
          multipliers(i).is_flip := false.B
          multipliers(i).is_neg := false.B
        }
      }
      val result_regs = RegInit(VecInit.fill(2)(VecInit.fill(w)(0.U.asTypeOf(new ComplexNum(bw)))))
      for(j <- 0 until 2) {
        if(j == 0) {
          for (i <- 0 until w) {
            result_regs(0)(i) := multipliers(i).out
          }
        }else{
          result_regs(j) := result_regs(j-1)
        }
      }
      io.out := result_regs(1)
      Twid_ROM.in_addr := cnt
    }
  }

  class TwiddleFactorsStreamed_mr_v2(N: Int, s: Int, r: Int, w: Int, bw: Int, delay: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en_main = Input(Bool())
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = Permutations.T2_rs(N, s, r)
    var mults = 0
    val Twid_ROM = Module(new TwiddleFactorROM_mr_forStreaming(N, s, r, w, bw)).io
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
    } else {
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

  class TwiddleFactorsStreamed_and_Iterative(N: Int, r:Int, w: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_twid_data = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec(2, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
      val out_cnt = Output(UInt(log2Ceil(N/w).W))
    })
    val list_of_twids = Permutations.T2(N,r)
    val initial_twid = for(i <- 0 until N) yield{
      cmplx(1,0)
    }
    val adjusted_rom_array = for(i <- 0 until (Math.log10(N)/Math.log10(r)).round.toInt) yield{
      if(i == 0){
        initial_twid.toArray
      }else{
        list_of_twids(i-1)
      }
    }
    var mults = 0
    for(j <- 0 until (Math.log10(N)/Math.log10(r)).round.toInt) {
      for (i <- 0 until N) {
        if (adjusted_rom_array(j)(i).re.abs > 0.00000001 && adjusted_rom_array(j)(i).im.abs > 0.00000001) {
          mults += 1
        }
      }
    }
    if(mults > 0) {
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new FPComplexMult(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt === ((N/w)-1).U){
          cnt := 0.U
        }.otherwise{
          cnt := cnt + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in_a := io.in(i)
          multipliers(i).in_b := io.in_twid_data(i)
        }
      }.otherwise{
        for(i <- 0 until w){
          multipliers(i).in_a := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_b := 0.U.asTypeOf(new ComplexNum(bw))
        }
        cnt := 0.U
      }
      for(i <- 0 until w){
        io.out(i) := multipliers(i).out_s
      }
      io.out_cnt := cnt
    }else{
      val is_enabled = RegInit(false.B)
      val cnt = RegInit(0.U(log2Ceil(N/w).W))
      val multipliers = for (i <- 0 until w) yield {
        val instance = Module(new ComplexNum_AdjustOrder(bw)).io
        instance
      }
      when(io.in_en.asUInt.orR){
        when(cnt === ((N/w)-1).U){
          cnt := 0.U
        }.otherwise{
          cnt := cnt + 1.U
        }
        for(i <- 0 until w){
          multipliers(i).in := io.in(i)
          multipliers(i).in_adj := 0.U
          when(io.in_twid_data(i).Re(bw-2,0) === convert_string_to_IEEE_754("1.0", bw).U){
            multipliers(i).is_flip := false.B
            when(io.in_twid_data(i).Re(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }.otherwise{
            multipliers(i).is_flip := true.B
            when(io.in_twid_data(i).Im(bw-1) === 1.U){
              multipliers(i).is_neg := true.B
            }.otherwise{
              multipliers(i).is_neg := false.B
            }
          }
        }
      }.otherwise{
        cnt := 0.U
        for(i <- 0 until w){
          multipliers(i).in := 0.U.asTypeOf(new ComplexNum(bw))
          multipliers(i).in_adj := 0.U
          multipliers(i).is_flip := false.B
          multipliers(i).is_neg := false.B
        }
      }
      io.out_cnt := cnt
      val result_regs = RegInit(VecInit.fill(2)(VecInit.fill(w)(0.U.asTypeOf(new ComplexNum(bw)))))
      for(j <- 0 until 2) {
        if(j == 0) {
          for (i <- 0 until w) {
            result_regs(0)(i) := multipliers(i).out
          }
        }else{
          result_regs(j) := result_regs(j-1)
        }
      }
      io.out := result_regs(1)
    }
  }

  class TwiddleFactors(N: Int, r:Int, w: Int, stage:Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val locations = mutable.ArrayBuffer[(Int, Int )]()
    for(i <- 0 until TotalStages-1){
      for(j <- 0 until N){
        if(Permutations.T2(N,r)(i)(j).re == Permutations.T2(N,r)(i)(j).im){
          val possibility = j
          for(k <- i + 1 until TotalStages){
            if(Permutations.T2(N,r)(k)(j).re == Permutations.T2(N,r)(k)(j).im){
            }
          }
        }
      }
    }
    val DFTr_Constants = Permutations.T2(N,r)(stage)
    var mults = 0
    for(i <- 0 until N){
      if(DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001){
        mults += 1
      }
    }
    val addregs = if(mults > 0){true}else{false} // it seems not too hard to optimize, but I'll come back to this since it is simpler than mixed-radix streaming
    val multipliers = for(i <- 0 until N) yield{
      val instance = Module(new FPComplexMult_reducable_forSymmetric(bw, DFTr_Constants(i).re, DFTr_Constants(i).im, addregs, false)).io
      instance
    }
    for(i <- 0 until N){
      multipliers(i).in_a := io.in(i)
      multipliers(i).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(i).re.toString, bw).U
      multipliers(i).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(i).im.toString, bw).U
      io.out(i) := multipliers(i).out_s
    }
  }

  class TwiddleFactors_v2(N: Int, r: Int, w: Int, stage: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val locations = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until TotalStages - 1) {
      for (j <- 0 until N) {
        if (Permutations.T2(N, r)(i)(j).re == Permutations.T2(N, r)(i)(j).im) {
          val possibility = j
          for (k <- i + 1 until TotalStages) {
            if (Permutations.T2(N, r)(k)(j).re == Permutations.T2(N, r)(k)(j).im) {
            }
          }
        }
      }
    }
    val DFTr_Constants = Permutations.T2(N, r)(stage)
    var mults = 0
    for (i <- 0 until N) {
      if (DFTr_Constants(i).re.abs > 0.00000001 && DFTr_Constants(i).im.abs > 0.00000001) {
        mults += 1
      }
    }
    val addregs = if (mults > 0) {
      true
    } else {
      false
    } // it seems not too hard to optimize, but I'll come back to this since it is simpler than mixed-radix streaming
    val multipliers = for (i <- 0 until N) yield {
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

  class TwiddleFactors_NoReduction(N: Int, r:Int, w: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_twid_data = Input(Vec(N, new ComplexNum(bw)))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val multipliers = for(i <- 0 until N) yield{
      val instance = Module(new FPComplexMult(bw)).io
      instance
    }
    for(i <- 0 until N){
      multipliers(i).in_a := io.in(i)
      multipliers(i).in_b.Re := io.in_twid_data(i).Re
      multipliers(i).in_b.Im := io.in_twid_data(i).Im
      io.out(i) := multipliers(i).out_s
    }
  }

  class TwiddleFactors_mr(N: Int, r:Int, s: Int, w: Int, bw: Int) extends Module { //mixed radix twiddle factors
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val DFTr_Constants = Permutations.T2_rs(N,s,r)
    var mult_count = 0
    val mult_ind = mutable.ArrayBuffer[Int]()
    val mult_needs = for(i <- 0 until w) yield{
      val n  = DFTr_Constants(i)
      val c1 = FFT.isReducable(n.re.abs)
      val c2 = FFT.isReducable(n.im.abs)
      if(!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))){
        mult_count += 1
        mult_ind += (i)
        (i, 0, false, false, true)
      }else{
        mult_ind += (i)
        if((c1._1 && n.im.abs < 0.005)){
          (i, c1._2.abs, n.re < -0.0005, false, false)
        }else{
          (i, c2._2.abs, n.im < -0.0005, true, false)
        }
      }
    }
    val CMultLatency = 2
    var T_latency = CMultLatency
    if(mult_count == 0){
      T_latency = 0
    }
    val TwiddleFactorConstantsRe = VecInit(Permutations.T2_rs(N,s,r).map(x=>convert_string_to_IEEE_754(x.re.toString, bw).U))
    val TwiddleFactorConstantsIm = VecInit(Permutations.T2_rs(N,s,r).map(x=>convert_string_to_IEEE_754(x.im.toString, bw).U))
    val TotalCycles = N/w
    val offset = w
    val adj = mutable.ArrayBuffer[(Int, Int, Boolean, Boolean, Boolean)]()
    val mult_ind_adj = mutable.ArrayBuffer[Int]()
    val adj_non_mult = mutable.ArrayBuffer[(Int, Int, Boolean, Boolean, Boolean)]()
    val mult_ind_adj_non_mult = mutable.ArrayBuffer[Int]()
    for(i <- 0 until w){
      if(mult_needs(i)._5){
        adj += mult_needs(i)
        mult_ind_adj += mult_ind(i)
      }else{
        adj_non_mult += mult_needs(i)
        mult_ind_adj_non_mult += mult_ind(i)
      }
    }
    var cmplx_adjusts = for(i <- 0 until w - mult_count) yield{
      val cm = Module(new ComplexNum_AdjustOrder(bw)).io
      cm
    }
    val adj_wire = Wire(Vec(w, new ComplexNum(bw)))
    for(i <- 0 until w - mult_count){
      cmplx_adjusts(i).in := io.in(adj_non_mult(i)._1)
      cmplx_adjusts(i).in_adj := adj_non_mult(i)._2.U
      cmplx_adjusts(i).is_neg := adj_non_mult(i)._3.B
      cmplx_adjusts(i).is_flip := adj_non_mult(i)._4.B
      adj_wire(mult_ind_adj_non_mult(i)) := cmplx_adjusts(i).out
    }
    for(i <- 0 until mult_count){
      adj_wire(mult_ind_adj(i)) := io.in(adj(i)._1)
    }
    if(mult_count != 0) {
      val FPMults = (for (i <- 0 until mult_count) yield {
        val fpmult = Module(new FPComplexMult_reducable_SimpleCases(bw, DFTr_Constants(adj(i)._1).re, DFTr_Constants(adj(i)._1).im)).io
        fpmult
      }).toVector
      for (k <- 0 until mult_count) {
        FPMults(k).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(adj(k)._1).re.toString, bw).U
        FPMults(k).in_a := adj_wire(mult_ind_adj(k)) //io.in(mult_ind_adj(k))
        FPMults(k).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(adj(k)._1).im.toString, bw).U
      }
      val total_reg = (w - mult_count)
      val reg_syncs = (for (i <- 0 until CMultLatency) yield {
        val regs = RegInit(VecInit.fill(total_reg)(0.U.asTypeOf(new ComplexNum(bw))))
        regs
      }).toVector
      for (i <- 0 until CMultLatency) {
        if (i == 0) {
          for (j <- 0 until w-mult_count){
            reg_syncs(0)(j) := adj_wire(mult_ind_adj_non_mult(j))
          }
        } else {
          reg_syncs(i) := reg_syncs(i - 1)
        }
      }
      val mult_results = Wire(Vec(w, new ComplexNum(bw)))
      for(i <- 0 until (w - mult_count)){
        mult_results(adj_non_mult(i)._1) := reg_syncs(CMultLatency-1)(i)
      }
      for(i <- 0 until mult_count){
        mult_results(adj(i)._1) := FPMults(i).out_s
      }
      for(i <- 0 until w){
        io.out(i) := mult_results(i)
      }
    }else{
      io.out := adj_wire
    }
  }
}
