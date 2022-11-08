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
import TwidFactorDesigns._
import PermutationDesigns._
import DFTDesigns._

import scala.collection.mutable

object FFTMRDesigns {
  // v2 versions have enable input
  class FFT_MixedRadix_Streaming_v2(N: Int, nr: Int, ns: Int, r: Int, s: Int, w: Int, bw: Int, inv: Boolean) extends Module { // streaming single radix fft, still in progress
    val w1 = w // this will be the input and output width
    var w2 = Permutations.getw2(w1, N, nr, ns, s)
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_last = Input(Bool())
      val in_ready = Input(Bool())
      val in_valid = Input(Bool())
      val out_ready = Output(Bool())
      val out_validate = Output(Bool())
      val out = Output(Vec(w, new ComplexNum(bw)))
      val out_last = Output(Bool())
    })
    var delay_cycles_stage2 = N / w1 - N / w2 // w2 will be bigger than w1
    // for the first N/w2 clock cycles, the output from the permutation should have a corresponding valid signal,
    // however, for the remaint N/w1-N/w2 clock cycles, the output from the permutation should have an invalid signal
    val CMultLatency = 2
    val T_L = CMultLatency
    var fftlatency1 = 0
    var fftlatency2 = 0
    val perm_latency1 = (N / w1) * 2
    var total_latency = getfftstreamingmrlatency(N,nr,ns,r,s,w,bw,inv)
    // 1. the w1<nr,w2<ns // already did
    // 2. the w1<nr, w2>=ns // not done yet
    // 3. the w1>=nr, w2<ns // not done yet
    // 4. the w1>=nr, w2>=ns // already did
    if (w1 < nr && w2 < ns) { //streaming width is less the a single FFTnr size - will have to be streamed as well \\ additionally this can only be the case if nr is greater than fundamental dft2 and 3 sizes
      val last_regs = RegInit(VecInit.fill(total_latency)(false.B))
      when(io.in_ready){
        last_regs(0) := io.in_last
        for(i <- 0 until total_latency-1) {
          last_regs(i + 1) := last_regs(i)
        }
      }
      io.out_last := last_regs(total_latency-1)
      val out_strt_cnt_reg = RegInit(true.B)
      when(io.in_ready){
        io.out_ready := out_strt_cnt_reg
        out_strt_cnt_reg := true.B
      }.otherwise{
        io.out_ready := false.B
      }
      println("case 1")
      fftlatency1 = getfftstreamedlatency(nr, r, w1, bw, inv)
      fftlatency2 = getfftstreamedlatency(ns, s, w2, bw, inv)
      val FFT_modules1 = Module(new FFT_SingleRadix_Streaming_NRO_v2(nr, r, w1, bw, inv)).io
      val FFT_modules2 = Module(new FFT_SingleRadix_Streaming_NRO_v2(ns, s, w2, bw, inv)).io // it seems that we have already accounted for the case where ns > s
      val Perm_module1 = Module(new PermutationsWithStreaming_v2(N, ns, ns, w1, 0, bw)).io
      val Perm_module2 = Module(new PermutationsWithStreaming_mr_v2(N, nr, nr, w2, w1, 0, bw, delay_cycles_stage2)).io
      val Perm_module3 = Module(new PermutationsWithStreaming_mr_v2(N, ns, ns, w1, w2, 0, bw, delay_cycles_stage2)).io
      val Twid_Modules = Module(new TwiddleFactorsStreamed_mr_v2(N, nr, ns, w2, bw, ((N / w1)), inv)).io
      FFT_modules1.in_en := io.in_ready
      FFT_modules2.in_en := io.in_ready
      Perm_module1.in_en_main := io.in_ready
      Perm_module2.in_en_main := io.in_ready
      Perm_module3.in_en_main := io.in_ready
      Twid_Modules.in_en_main := io.in_ready

      val DFT_regdelays1 = RegInit(VecInit.fill(fftlatency1)(false.B)) // we might not need  the dft regdelays for the streamed ffts (they already use it)
      val DFT_regdelays2 = RegInit(VecInit.fill(fftlatency2)(false.B))
      val Twid_regdelays = RegInit(VecInit.fill(T_L)(false.B))
      val Perm_regdelays1 = RegInit(VecInit.fill(3)(VecInit.fill(perm_latency1)(false.B)))
      val out_regdelay = RegInit(false.B)
      val results = RegInit(VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw))))
      for (i <- 0 until 3) {
        for (j <- 0 until perm_latency1) {
          if (j == 0) {
            if (i == 0) {
              when(io.in_ready) {
                Perm_regdelays1(0)(0) := io.in_valid
              }
              Perm_module1.in_en(0) := io.in_valid
              Perm_module1.in := io.in
            } else if (i == 1) {
              when(io.in_ready) {
                Perm_regdelays1(1)(0) := FFT_modules1.out_validate
              }
              Perm_module2.in_en(0) := FFT_modules1.out_validate
              Perm_module2.in := FFT_modules1.out
            } else {
              when(io.in_ready) {
                Perm_regdelays1(2)(0) := FFT_modules2.out_validate
              }
              Perm_module3.in_en(0) := FFT_modules2.out_validate
              Perm_module3.in := FFT_modules2.out
            }
          } else {
            when(io.in_ready) {
              Perm_regdelays1(i)(j) := Perm_regdelays1(i)(j - 1)
            }
            if (i == 0) {
              Perm_module1.in_en(j) := Perm_regdelays1(0)(j - 1)
            } else if (i == 1) {
              Perm_module2.in_en(j) := Perm_regdelays1(1)(j - 1)
            } else {
              Perm_module3.in_en(j) := Perm_regdelays1(2)(j - 1)
            }
            if (j == perm_latency1 - 1) {
              if (i == 0) {
                FFT_modules1.in_ready := Perm_regdelays1(0)(perm_latency1 - 1)
                Perm_module1.in_en(perm_latency1) := Perm_regdelays1(0)(perm_latency1 - 1)
                FFT_modules1.in := Perm_module1.out
              } else if (i == 1) {
                Perm_module2.in_en(perm_latency1) := Perm_regdelays1(1)(perm_latency1 - 1)
              } else {
                when(io.in_ready) {
                  out_regdelay := Perm_regdelays1(2)(perm_latency1 - 1)
                }
                Perm_module3.in_en(perm_latency1) := Perm_regdelays1(2)(perm_latency1 - 1)
              }
            }
          }
        }
      }
      for (i <- 0 until T_L) {
        if (i == 0) {
          when(io.in_ready) {
            Twid_regdelays(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          }
          Twid_Modules.in_en(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          Twid_Modules.in := Perm_module2.out
        } else {
          when(io.in_ready) {
            Twid_regdelays(i) := Twid_regdelays(i - 1)
          }
          Twid_Modules.in_en(i) := Twid_regdelays(i - 1)
          if (i == T_L - 1) {
            FFT_modules2.in_ready := Twid_regdelays(T_L - 1)
            FFT_modules2.in := Twid_Modules.out
          }
        }
      }
      when(io.in_ready) {
        when(Perm_regdelays1(2)(perm_latency1 - 1)) {
          results := Perm_module3.out
        }.otherwise {
          for (i <- 0 until w1) {
            results(i) := 0.U.asTypeOf(new ComplexNum(bw))
          }
        }
      }
      io.out := results
      io.out_validate := out_regdelay // i think this is all we need for this case
    } else if (w1 < nr && w2 >= ns) {
      val last_regs = RegInit(VecInit.fill(total_latency)(false.B))
      when(io.in_ready){
        last_regs(0) := io.in_last
        for(i <- 0 until total_latency-1) {
          last_regs(i + 1) := last_regs(i)
        }
      }
      io.out_last := last_regs(total_latency-1)
      val out_strt_cnt_reg = RegInit(true.B)
      when(io.in_ready) {
        io.out_ready := out_strt_cnt_reg
        out_strt_cnt_reg := true.B
      }.otherwise {
        io.out_ready := false.B
      }
      println("case 2")
      fftlatency1 = getfftstreamedlatency(nr, r, w1, bw, inv) // in this case the fft1 is reduced streaming width
      fftlatency2 = getFFTLatency(ns, s, ns, bw, inv) // the fft2 is full streaming width
      val FFT_modules1 = Module(new FFT_SingleRadix_Streaming_NRO_v2(nr, r, w1, bw, inv)).io
      //same case with the other sets of FFT modules
      println(s"${(ns,s)}")
      val FFT_modules2 = if (ns == s) { // it could either be that ns == s
        val row = for (j <- 0 until w2 / ns) yield { // dft modules
          val instance = Module(new DFT_Symmetric_NRV_v2(s, bw, inv)).io
          instance
        }
        row
      } else { // or it is greater
        val row = for (j <- 0 until w2 / ns) yield {
          val instance = Module(new FFT_SingleRadix_NRV_v2(ns, s, ns, bw, inv)).io
          instance
        }
        row
      }
      val Perm_module1 = Module(new PermutationsWithStreaming_v2(N, ns, ns, w1, 0, bw)).io //  no changes needed yet
      val Perm_module2 = Module(new PermutationsWithStreaming_mr_v2(N, nr, nr, w2, w1, 0, bw, delay_cycles_stage2)).io
      val Perm_module3 = Module(new PermutationsWithStreaming_mr_v2(N, ns, ns, w1, w2, 0, bw, delay_cycles_stage2)).io
      val Twid_Modules = Module(new TwiddleFactorsStreamed_mr_v2(N, nr, ns, w2, bw, ((N / w1)), inv)).io
      FFT_modules1.in_en := io.in_ready
      for(i <- 0 until w2/ns){
        FFT_modules2(i).in_en := io.in_ready
      }
      Perm_module1.in_en_main := io.in_ready
      Perm_module2.in_en_main := io.in_ready
      Perm_module3.in_en_main := io.in_ready
      Twid_Modules.in_en_main := io.in_ready



      val DFT_regdelays1 = RegInit(VecInit.fill(fftlatency1)(false.B))
      val DFT_regdelays2 = RegInit(VecInit.fill(fftlatency2)(false.B))
      val Twid_regdelays = RegInit(VecInit.fill(T_L)(false.B)) // twid has latency of 2
      val Perm_regdelays1 = RegInit(VecInit.fill(3)(VecInit.fill(perm_latency1)(false.B))) // there are 3 permutation modules
      val out_regdelay = RegInit(false.B) // this is the output register for validate
      val results = RegInit(VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))) // here is the output registers for outputs
      for (i <- 0 until 3) {
        for (j <- 0 until perm_latency1) {
          if (j == 0) {
            if (i == 0) {
              when(io.in_ready) {
                Perm_regdelays1(0)(0) := io.in_valid
              }
              Perm_module1.in_en(0) := io.in_valid
              Perm_module1.in := io.in
            } else if (i == 1) {
              when(io.in_ready) {
                Perm_regdelays1(1)(0) := DFT_regdelays1(fftlatency1 - 1)
              }
              Perm_module2.in_en(0) := DFT_regdelays1(fftlatency1 - 1)
              Perm_module2.in := FFT_modules1.out
            } else {
              when(io.in_ready) {
                Perm_regdelays1(2)(0) := DFT_regdelays2(fftlatency2 - 1)
              }
              Perm_module3.in_en(0) := DFT_regdelays2(fftlatency2 - 1)
              for (k <- 0 until w2 / ns) {
                for (l <- 0 until ns) {
                  Perm_module3.in(k * ns + l) := FFT_modules2(k).out(l)
                }
              }
            }
          } else {
            when(io.in_ready) {
              Perm_regdelays1(i)(j) := Perm_regdelays1(i)(j - 1)
            }
            if (i == 0) {
              Perm_module1.in_en(j) := Perm_regdelays1(0)(j - 1)
            } else if (i == 1) {
              Perm_module2.in_en(j) := Perm_regdelays1(1)(j - 1)
            } else {
              Perm_module3.in_en(j) := Perm_regdelays1(2)(j - 1)
            }
            if (j == perm_latency1 - 1) {
              if (i == 0) {
                Perm_module1.in_en(perm_latency1) := Perm_regdelays1(0)(perm_latency1 - 1)
              } else if (i == 1) {
                Perm_module2.in_en(perm_latency1) := Perm_regdelays1(1)(perm_latency1 - 1)
              } else {
                Perm_module3.in_en(perm_latency1) := Perm_regdelays1(2)(perm_latency1 - 1)
              }
            }
          }
        }
      }
      for (i <- 0 until T_L) {
        if (i == 0) {
          when(io.in_ready) {
            Twid_regdelays(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          }
          Twid_Modules.in_en(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          Twid_Modules.in := Perm_module2.out
        } else {
          when(io.in_ready) {
            Twid_regdelays(i) := Twid_regdelays(i - 1)
          }
          Twid_Modules.in_en(i) := Twid_regdelays(i - 1)
        }
      }

      for (i <- 0 until fftlatency1) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays1(0) := Perm_regdelays1(0)(perm_latency1 - 1)
          }
          FFT_modules1.in := Perm_module1.out
          FFT_modules1.in_ready := Perm_regdelays1(0)(perm_latency1 - 1)
        } else {
          when(io.in_ready) {
            DFT_regdelays1(i) := DFT_regdelays1(i - 1)
          }
        }
      }

      for (i <- 0 until fftlatency2) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays2(0) := Twid_regdelays(T_L - 1)
          }
          for (k <- 0 until w2 / ns) {
            for (l <- 0 until ns) {
              FFT_modules2(k).in(l) := Twid_Modules.out(k * ns + l)
            }
          }
        } else {
          when(io.in_ready) {
            DFT_regdelays2(i) := DFT_regdelays2(i - 1)
          }
        }
      }
      when(io.in_ready) {
        out_regdelay := Perm_regdelays1(2)(perm_latency1 - 1)
      }
      when(io.in_ready) {
        when(Perm_regdelays1(2)(perm_latency1 - 1)) {
          results := Perm_module3.out
        }.otherwise {
          results := VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))
        }
      }
      io.out_validate := out_regdelay
      io.out := results
    } else if (w1 >= nr && w2 < ns) {
      val last_regs = RegInit(VecInit.fill(total_latency)(false.B))
      when(io.in_ready){
        last_regs(0) := io.in_last
        for(i <- 0 until total_latency-1) {
          last_regs(i + 1) := last_regs(i)
        }
      }
      io.out_last := last_regs(total_latency-1)
      val out_strt_cnt_reg = RegInit(true.B)
      when(io.in_ready) {
        io.out_ready := out_strt_cnt_reg
        out_strt_cnt_reg := true.B
      }.otherwise {
        io.out_ready := false.B
      }
      println("case 3")
      fftlatency1 = getFFTLatency(nr, r, nr, bw, inv) // the fft1 is full streaming width
      fftlatency2 = getfftstreamedlatency(ns, s, w2, bw, inv) // the fft2 is a reduced streaming width
      val FFT_modules1 = if (nr == r) { // if this this the case then we just need to instantiate dft modules
        val row = for (j <- 0 until w1 / nr) yield { // since nr == r
          val instance = Module(new DFT_Symmetric_NRV_v2(r, bw,inv)).io
          instance
        }
        row
      } else { // otherwise we instantiate full streaming width fft modules
        val row = for (j <- 0 until w1 / nr) yield { // since nr != r // w is ideally a multiple of nr // also notice that this is based on the first streaming width
          val instance = Module(new FFT_SingleRadix_NRV_v2(nr, r, nr, bw,inv)).io
          instance
        }
        row
      }
      //same case with the other sets of FFT modules
      val FFT_modules2 = Module(new FFT_SingleRadix_Streaming_NRO_v2(ns, s, w2, bw, inv)).io // the streamed FFT2
      val Perm_module1 = Module(new PermutationsWithStreaming_v2(N, ns, ns, w1, 0, bw)).io
      val Perm_module2 = Module(new PermutationsWithStreaming_mr_v2(N, nr, nr, w2, w1, 0, bw, delay_cycles_stage2)).io
      val Perm_module3 = Module(new PermutationsWithStreaming_mr_v2(N, ns, ns, w1, w2, 0, bw, delay_cycles_stage2)).io
      val Twid_Modules = Module(new TwiddleFactorsStreamed_mr_v2(N, nr, ns, w2, bw, ((N / w1)), inv)).io
      for(i <- 0 until w1/nr) {
        FFT_modules1(i).in_en := io.in_ready
      }
      FFT_modules2.in_en := io.in_ready
      Perm_module1.in_en_main := io.in_ready
      Perm_module2.in_en_main := io.in_ready
      Perm_module3.in_en_main := io.in_ready
      Twid_Modules.in_en_main := io.in_ready

      val DFT_regdelays1 = RegInit(VecInit.fill(fftlatency1)(false.B))
      val DFT_regdelays2 = RegInit(VecInit.fill(fftlatency2)(false.B))
      val Twid_regdelays = RegInit(VecInit.fill(T_L)(false.B)) // twid has latency of 2
      val Perm_regdelays1 = RegInit(VecInit.fill(3)(VecInit.fill(perm_latency1)(false.B))) // there are 3 permutation modules
      val out_regdelay = RegInit(false.B) // this is the output register for validate
      val results = RegInit(VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))) // here is the output registers for outputs
      for (i <- 0 until 3) {
        for (j <- 0 until perm_latency1) {
          if (j == 0) {
            if (i == 0) {
              when(io.in_ready) {
                Perm_regdelays1(0)(0) := io.in_valid
              }
              Perm_module1.in_en(0) := io.in_valid
              Perm_module1.in := io.in
            } else if (i == 1) {
              when(io.in_ready) {
                Perm_regdelays1(1)(0) := DFT_regdelays1(fftlatency1 - 1)
              }
              Perm_module2.in_en(0) := DFT_regdelays1(fftlatency1 - 1)
              for (k <- 0 until w1 / nr) {
                for (l <- 0 until nr) {
                  Perm_module2.in(k * nr + l) := FFT_modules1(k).out(l)
                }
              }
            } else {
              when(io.in_ready) {
                Perm_regdelays1(2)(0) := DFT_regdelays2(fftlatency2 - 1)
              }
              Perm_module3.in_en(0) := DFT_regdelays2(fftlatency2 - 1)
              Perm_module3.in := FFT_modules2.out
            }
          } else {
            when(io.in_ready) {
              Perm_regdelays1(i)(j) := Perm_regdelays1(i)(j - 1)
            }
            if (i == 0) {
              Perm_module1.in_en(j) := Perm_regdelays1(0)(j - 1)
            } else if (i == 1) {
              Perm_module2.in_en(j) := Perm_regdelays1(1)(j - 1)
            } else {
              Perm_module3.in_en(j) := Perm_regdelays1(2)(j - 1)
            }
            if (j == perm_latency1 - 1) {
              if (i == 0) {
                Perm_module1.in_en(perm_latency1) := Perm_regdelays1(0)(perm_latency1 - 1)
              } else if (i == 1) {
                Perm_module2.in_en(perm_latency1) := Perm_regdelays1(1)(perm_latency1 - 1)
              } else {
                Perm_module3.in_en(perm_latency1) := Perm_regdelays1(2)(perm_latency1 - 1)
              }
            }
          }
        }
      }
      for (i <- 0 until T_L) {
        if (i == 0) {
          when(io.in_ready) {
            Twid_regdelays(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          }
          Twid_Modules.in_en(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          Twid_Modules.in := Perm_module2.out
        } else {
          when(io.in_ready) {
            Twid_regdelays(i) := Twid_regdelays(i - 1)
          }
          Twid_Modules.in_en(i) := Twid_regdelays(i - 1)
        }
      }

      for (i <- 0 until fftlatency1) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays1(0) := Perm_regdelays1(0)(perm_latency1 - 1)
          }
          for (k <- 0 until w1 / nr) {
            for (l <- 0 until nr) {
              FFT_modules1(k).in(l) := Perm_module1.out(k * nr + l)
            }
          }
        } else {
          when(io.in_ready) {
            DFT_regdelays1(i) := DFT_regdelays1(i - 1)
          }
        }
      }

      for (i <- 0 until fftlatency2) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays2(0) := Twid_regdelays(T_L - 1)
          }
          FFT_modules2.in_ready := Twid_regdelays(T_L - 1)
          FFT_modules2.in := Twid_Modules.out
        } else {
          when(io.in_ready) {
            DFT_regdelays2(i) := DFT_regdelays2(i - 1)
          }
        }
      }
      when(io.in_ready) {
        out_regdelay := Perm_regdelays1(2)(perm_latency1 - 1)
      }
      when(io.in_ready) {
        when(Perm_regdelays1(2)(perm_latency1 - 1)) {
          results := Perm_module3.out
        }.otherwise {
          results := VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))
        }
      }
      io.out_validate := out_regdelay
      io.out := results
    } else if (w1 >= nr && w2 >= ns) { // streaming width is greater than or equal to the FFTnr size (this also implies the same case for the FFTns size as well)
      val last_regs = RegInit(VecInit.fill(total_latency)(false.B))
      when(io.in_ready){
        last_regs(0) := io.in_last
        for(i <- 0 until total_latency-1) {
          last_regs(i + 1) := last_regs(i)
        }
      }
      io.out_last := last_regs(total_latency-1)
      val out_strt_cnt_reg = RegInit(true.B)
      when(io.in_ready) {
        io.out_ready := out_strt_cnt_reg
        out_strt_cnt_reg := true.B
      }.otherwise {
        io.out_ready := false.B
      }
      println("case 4")
      fftlatency1 = getFFTLatency(nr, r, nr, bw, inv) // this function should also give the DFT_NRV latency as well if we set the nr==r
      fftlatency2 = getFFTLatency(ns, s, ns, bw, inv)
      val FFT_modules1 = if (nr == r) { // if this this the case then we just need to instantiate dft modules
        val row = for (j <- 0 until w1 / nr) yield { // since nr == r
          val instance = Module(new DFT_Symmetric_NRV_v2(r, bw, inv)).io
          instance
        }
        row
      } else { // otherwise we instantiate full streaming width fft modules
        val row = for (j <- 0 until w1 / nr) yield { // since nr != r // w is ideally a multiple of nr // also notice that this is based on the first streaming width
          val instance = Module(new FFT_SingleRadix_NRV_v2(nr, r, nr, bw, inv)).io
          instance
        }
        row
      }
      //same case with the other sets of FFT modules
      val FFT_modules2 = if (ns == s) {
        val row = for (j <- 0 until w2 / ns) yield { // dft modules
          val instance = Module(new DFT_Symmetric_NRV_v2(s, bw, inv)).io
          instance
        }
        row
      } else {
        val row = for (j <- 0 until w2 / ns) yield {
          val instance = Module(new FFT_SingleRadix_NRV_v2(ns, s, ns, bw, inv)).io
          instance
        }
        row
      }
      // so there is always a total of w1/nr dft modules in the first stage and w2/ns dft modules in the second stage
      val Perm_module1 = Module(new PermutationsWithStreaming_v2(N, ns, ns, w1, 0, bw)).io
      val Perm_module2 = Module(new PermutationsWithStreaming_mr_v2(N, nr, nr, w2, w1, 0, bw, delay_cycles_stage2)).io
      val Perm_module3 = Module(new PermutationsWithStreaming_mr_v2(N, ns, ns, w1, w2, 0, bw, delay_cycles_stage2)).io
      val Twid_Modules = Module(new TwiddleFactorsStreamed_mr_v2(N, nr, ns, w2, bw, ((N / w1)), inv)).io
      for(i <- 0 until w1/nr){
        FFT_modules1(i).in_en := io.in_ready
      }
      for(i <- 0 until w2/ns){
        FFT_modules2(i).in_en := io.in_ready
      }
      Perm_module1.in_en_main := io.in_ready
      Perm_module2.in_en_main := io.in_ready
      Perm_module3.in_en_main := io.in_ready
      Twid_Modules.in_en_main := io.in_ready


      val DFT_regdelays1 = RegInit(VecInit.fill(fftlatency1)(false.B))
      val DFT_regdelays2 = RegInit(VecInit.fill(fftlatency2)(false.B))
      val Twid_regdelays = RegInit(VecInit.fill(T_L)(false.B)) // twid has latency of 2
      val Perm_regdelays1 = RegInit(VecInit.fill(3)(VecInit.fill(perm_latency1)(false.B))) // there are 3 permutation modules
      val out_regdelay = RegInit(false.B) // this is the output register for validate
      val results = RegInit(VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))) // here is the output registers for outputs
      for (i <- 0 until 3) {
        for (j <- 0 until perm_latency1) {
          if (j == 0) {
            if (i == 0) {
              when(io.in_ready) {
                Perm_regdelays1(0)(0) := io.in_valid
              }
              Perm_module1.in_en(0) := io.in_valid
              Perm_module1.in := io.in
            } else if (i == 1) {
              when(io.in_ready) {
                Perm_regdelays1(1)(0) := DFT_regdelays1(fftlatency1 - 1)
              }
              Perm_module2.in_en(0) := DFT_regdelays1(fftlatency1 - 1)
              for (k <- 0 until w1 / nr) {
                for (l <- 0 until nr) {
                  Perm_module2.in(k * nr + l) := FFT_modules1(k).out(l)
                }
              }
            } else {
              when(io.in_ready) {
                Perm_regdelays1(2)(0) := DFT_regdelays2(fftlatency2 - 1)
              }
              Perm_module3.in_en(0) := DFT_regdelays2(fftlatency2 - 1)
              for (k <- 0 until w2 / ns) {
                for (l <- 0 until ns) {
                  Perm_module3.in(k * ns + l) := FFT_modules2(k).out(l)
                }
              }
            }
          } else {
            when(io.in_ready) {
              Perm_regdelays1(i)(j) := Perm_regdelays1(i)(j - 1)
            }
            if (i == 0) {
              Perm_module1.in_en(j) := Perm_regdelays1(0)(j - 1)
            } else if (i == 1) {
              Perm_module2.in_en(j) := Perm_regdelays1(1)(j - 1)
            } else {
              Perm_module3.in_en(j) := Perm_regdelays1(2)(j - 1)
            }
            if (j == perm_latency1 - 1) {
              if (i == 0) {
                Perm_module1.in_en(perm_latency1) := Perm_regdelays1(0)(perm_latency1 - 1)
              } else if (i == 1) {
                Perm_module2.in_en(perm_latency1) := Perm_regdelays1(1)(perm_latency1 - 1)
              } else {
                Perm_module3.in_en(perm_latency1) := Perm_regdelays1(2)(perm_latency1 - 1)
              }
            }
          }
        }
      }
      for (i <- 0 until T_L) {
        if (i == 0) {
          when(io.in_ready) {
            Twid_regdelays(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          }
          Twid_Modules.in_en(0) := Perm_regdelays1(1)(perm_latency1 - 1)
          Twid_Modules.in := Perm_module2.out
        } else {
          when(io.in_ready) {
            Twid_regdelays(i) := Twid_regdelays(i - 1)
          }
          Twid_Modules.in_en(i) := Twid_regdelays(i - 1)
        }
      }

      for (i <- 0 until fftlatency1) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays1(0) := Perm_regdelays1(0)(perm_latency1 - 1)
          }
          for (k <- 0 until w1 / nr) {
            for (l <- 0 until nr) {
              FFT_modules1(k).in(l) := Perm_module1.out(k * nr + l)
            }
          }
        } else {
          when(io.in_ready) {
            DFT_regdelays1(i) := DFT_regdelays1(i - 1)
          }
        }
      }

      for (i <- 0 until fftlatency2) {
        if (i == 0) {
          when(io.in_ready) {
            DFT_regdelays2(0) := Twid_regdelays(T_L - 1)
          }
          for (k <- 0 until w2 / ns) {
            for (l <- 0 until ns) {
              FFT_modules2(k).in(l) := Twid_Modules.out(k * ns + l)
            }
          }
        } else {
          when(io.in_ready) {
            DFT_regdelays2(i) := DFT_regdelays2(i - 1)
          }
        }
      }
      when(io.in_ready) {
        out_regdelay := Perm_regdelays1(2)(perm_latency1 - 1)
      }
      when(io.in_ready) {
        when(Perm_regdelays1(2)(perm_latency1 - 1)) {
          results := Perm_module3.out
        }.otherwise {
          results := VecInit.fill(w1)(0.U.asTypeOf(new ComplexNum(bw)))
        }
      }
      io.out_validate := out_regdelay
      io.out := results
    }
  }

}
