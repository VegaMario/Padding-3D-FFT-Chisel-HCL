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
import FFTMRDesigns._
import TwidFactorDesigns._
import PermutationDesigns._
import DFTDesigns._

import scala.collection.mutable

object FFTSRDesigns {

  class FFT_SingleRadix_NRV_v2(N: Int, r: Int, w: Int, bw: Int, inv: Boolean) extends Module { //FFT no ready and validate input/output
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(N, new ComplexNum(bw)))
      val out = Output(Vec(N, new ComplexNum(bw)))
    })
    val dft_T_ltncy = compute_latency(N,r,w,inv, false)
    val CMultLatency = 2
    val DFTs_per_stage = N / r
    val number_of_stages = (Math.log10(N) / Math.log10(r)).round.toInt
    val Twid_latency = (N / w) * CMultLatency
    val Perm_latency = 0
    val Total_Latency = dft_T_ltncy._2 + (number_of_stages) * dft_T_ltncy._1 + (number_of_stages + 1) * Perm_latency + 1
    val DFT_instances = (for (i <- 0 until number_of_stages) yield {
      val DFT_instnace_row = (for (j <- 0 until DFTs_per_stage) yield {
        val DFT_instance = Module(new DFT_Symmetric_NRV_v2(r, bw, inv)).io
        DFT_instance
      }).toVector
      DFT_instnace_row
    }).toVector
    for(i <- 0 until number_of_stages){
      for(j <- 0 until DFTs_per_stage){
        DFT_instances(i)(j).in_en := io.in_en
      }
    }
    val Input_Permutation = Module(new PermutationsSimple(N, r, 1, bw)).io
    val Stage_Permutations = (for (i <- 0 until number_of_stages) yield {
      val stage_permutation = Module(new PermutationsSimple(N, r, 0, bw)).io
      stage_permutation
    }).toVector
    val TwiddleFactorModules = (for (i <- 0 until number_of_stages - 1) yield {
      val Twid = Module(new TwiddleFactors_v2(N, r, N, i, bw, inv)).io
      Twid
    }).toVector
    for(i <- 0 until number_of_stages-1){
      TwiddleFactorModules(i).in_en := io.in_en
    }
    for (i <- 0 until N) {
      Input_Permutation.in(i) := io.in(i)
    }
    val results = WireInit(VecInit.fill(N)(0.U.asTypeOf(new ComplexNum(bw))))
    //val results = Reg(Vec(N, new ComplexNum(bw)))
    for (i <- 0 until number_of_stages) {
      for (j <- 0 until DFTs_per_stage) {
        for (k <- 0 until r) {
          if (i == 0) {
            DFT_instances(i)(j).in(k) := Input_Permutation.out(j * r + k)
          } else {
            DFT_instances(i)(j).in(k) := TwiddleFactorModules(i - 1).out(j * r + k)
          }
          Stage_Permutations(i).in(j * r + k) := DFT_instances(i)(j).out(k)
        }
      }
      if (i != 0) {
        TwiddleFactorModules(i - 1).in := Stage_Permutations(i - 1).out
      }
      if (i == number_of_stages - 1) {
        results := Stage_Permutations(i).out
      }
    }
    io.out := results
  }

  class FFT_SingleRadix_Streaming_NRO_v2(N: Int, r: Int, w: Int, bw: Int, inv: Boolean) extends Module { // streaming single radix fft, still in progress
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_ready = Input(Bool())
      val out_validate = Output(Bool())
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val dft_T_ltncy = compute_latency(N,r,w,inv, true)
    val DFT_latency = dft_T_ltncy._1
    val T_L = dft_T_ltncy._2
    val number_of_stages = (Math.log10(N) / Math.log10(r)).round.toInt
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val Twid_latency = TotalStages * T_L
    val Perm_latency = (N / w) * 2
    val Total_Latency = Twid_latency + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency + 1
    val DFT_regdelays = RegInit(VecInit.fill(number_of_stages)(VecInit.fill(DFT_latency)(false.B)))
    val Twid_regdelays = RegInit(VecInit.fill(number_of_stages - 1)(VecInit.fill(T_L)(false.B)))
    val Perm_regdelays = RegInit(VecInit.fill(number_of_stages + 1)(VecInit.fill(Perm_latency)(false.B)))
    val DFT_modules = for (i <- 0 until number_of_stages) yield {
      val row = for (j <- 0 until w / r) yield {
        val instance = Module(new DFT_Symmetric_NRV_v2(r, bw, inv)).io
        instance
      }
      row
    }
    for(i <- 0 until number_of_stages){
      for(j <- 0 until w/r){
        DFT_modules(i)(j).in_en := io.in_en
      }
    }
    val Perm_modules = for (i <- 0 until number_of_stages + 1) yield {
      if (i == 0) {
        val instance = Module(new PermutationsWithStreaming_v2(N, r, r, w, 1, bw)).io // bit reversal
        instance
      } else {
        val instance = Module(new PermutationsWithStreaming_v2(N, r, r, w, 0, bw)).io //stride permute
        instance
      }
    }
    for(i <- 0 until number_of_stages + 1){
      Perm_modules(i).in_en_main := io.in_en
    }
    val Twid_Modules = for (i <- 0 until number_of_stages - 1) yield {
      val instance = Module(new TwiddleFactorsStreamed_v2(N, r, w, i, bw, inv)).io
      instance
    }
    for(i <- 0 until number_of_stages-1){
      Twid_Modules(i).in_en_main := io.in_en
    }
    for (i <- 0 until number_of_stages + 1) {
      for (j <- 0 until Perm_latency) {
        if (j == 0) {
          if (i == 0) {
            when(io.in_en) {
              Perm_regdelays(0)(0) := io.in_ready
            }
            Perm_modules(0).in_en(0) := io.in_ready
            when(io.in_ready) {
              Perm_modules(0).in := io.in
            }.otherwise {
              for (l <- 0 until w) {
                Perm_modules(0).in(l) := 0.U.asTypeOf(new ComplexNum(bw))
              }
            }
          } else {
            when(io.in_en) {
              Perm_regdelays(i)(0) := DFT_regdelays(i - 1)(DFT_latency - 1)
            }
            Perm_modules(i).in_en(0) := DFT_regdelays(i - 1)(DFT_latency - 1)
            for (k <- 0 until w / r) {
              for (l <- 0 until r) {
                Perm_modules(i).in(k * r + l) := DFT_modules(i - 1)(k).out(l)
              }
            }
          }
        } else {
          when(io.in_en) {
            Perm_regdelays(i)(j) := Perm_regdelays(i)(j - 1)
          }
          Perm_modules(i).in_en(j) := Perm_regdelays(i)(j - 1)
          if (j == Perm_latency - 1) {
            Perm_modules(i).in_en((N / w) * 2) := Perm_regdelays(i)(Perm_latency - 1)
          }
        }
      }
    }
    for (i <- 0 until number_of_stages) {
      for (j <- 0 until DFT_latency) {
        if (j == 0) {
          if (i == 0) {
            when(io.in_en) {
              DFT_regdelays(0)(0) := Perm_regdelays(0)(Perm_latency - 1)
            }
            for (k <- 0 until w / r) {
              for (l <- 0 until r) {
                DFT_modules(0)(k).in(l) := Perm_modules(0).out(k * r + l)
              }
            }
          } else {
            when(io.in_en) {
              DFT_regdelays(i)(0) := Twid_regdelays(i - 1)(T_L - 1)
            }
            for (k <- 0 until w / r) {
              for (l <- 0 until r) {
                DFT_modules(i)(k).in(l) := Twid_Modules(i - 1).out(k * r + l)
              }
            }
          }
        } else {
          when(io.in_en) {
            DFT_regdelays(i)(j) := DFT_regdelays(i)(j - 1)
          }
        }
      }
    }
    for (i <- 0 until number_of_stages - 1) {
      for (j <- 0 until T_L) {
        if (j == 0) {
          if (i == 0) {
            when(io.in_en) {
              Twid_regdelays(0)(0) := Perm_regdelays(1)(Perm_latency - 1)
            }
            Twid_Modules(0).in_en(0) := Perm_regdelays(1)(Perm_latency - 1)
            Twid_Modules(0).in := Perm_modules(1).out
          } else {
            when(io.in_en) {
              Twid_regdelays(i)(0) := Perm_regdelays(i + 1)(Perm_latency - 1)
            }
            Twid_Modules(i).in_en(0) := Perm_regdelays(i + 1)(Perm_latency - 1)
            Twid_Modules(i).in := Perm_modules(i + 1).out
          }
        } else {
          when(io.in_en) {
            Twid_regdelays(i)(j) := Twid_regdelays(i)(j - 1)
          }
          Twid_Modules(i).in_en(j) := Twid_regdelays(i)(j - 1)
        }
      }
    }
    io.out_validate := Perm_regdelays(number_of_stages)(Perm_latency - 1) // the validate just outputs directly
    io.out := Perm_modules(number_of_stages).out // same case with the results
  }


}
