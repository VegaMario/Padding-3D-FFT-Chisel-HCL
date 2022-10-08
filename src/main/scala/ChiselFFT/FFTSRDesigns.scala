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

  class FFT_SingleRadix_NRV_v2(N: Int, r: Int, w: Int, bw: Int) extends Module { //FFT no ready and validate input/output
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(N, new ComplexNum(bw)))
      val out = Output(Vec(N, new ComplexNum(bw)))
    })
    val DFTr_Constants = FFT.DFT_gen(r).map(_.toVector).toVector
    var mult_count = 0
    for (i <- 0 until r - 1) {
      for (j <- 0 until r - 1) {
        val n = DFTr_Constants(i + 1)(j + 1)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
          mult_count += 1
        }
      }
    }
    val inner_square_size = r - 1
    var last_ind = r - 1
    var start_ind = 1
    val inner_inner_square_size = inner_square_size / 2
    val ind_sq = for (i <- 0 until inner_inner_square_size) yield {
      val row = for (j <- 0 until inner_inner_square_size) yield {
        if (((i + 1) * (j + 1) % r) > r / 2) {
          ((i + 1) * (j + 1) % r) - r
        } else {
          ((i + 1) * (j + 1) % r)
        }
      }
      row
    }
    val ind_sq_unique = mutable.ArrayBuffer[Int]()
    val ind_sq_unique_address = mutable.ArrayBuffer[(Int, Int)]()
    val cases = mutable.ArrayBuffer[(Int, Int)]()
    val cases_addr = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until inner_inner_square_size) {
      for (j <- 0 until inner_inner_square_size) {
        if (!ind_sq_unique.contains(ind_sq(i)(j).abs)) {
          ind_sq_unique += ind_sq(i)(j)
          val temp = (i, j)
          ind_sq_unique_address += temp
        }
        if (!cases.contains((ind_sq(i)(j).abs, j))) {
          val temp = (ind_sq(i)(j).abs, j)
          cases += temp
          val temp2 = (i, j)
          cases_addr += temp2
        }
      }
    }

    def returnMapping2(num: Int, arr: Array[(Int, Int)]): (Int, Int) = { //for multiply access values
      var returnval = (0, 0)
      for (i <- 0 until arr.length) {
        if (num == arr(i)._1) {
          returnval = ind_sq_unique_address(i)
        }
      }
      returnval
    }

    val cases_addr_adjusted = cases.map(x => returnMapping2(x._1.abs, ind_sq_unique.zipWithIndex.toArray))

    def returnMapping3(num: (Int, Int), arr: Array[(Int, Int)]): Int = {
      var returnval = 0
      for (i <- 0 until arr.length) {
        if (num == arr(i)) {
          returnval = i
        }
      }
      returnval
    }

    val new_adj_case_sq = ind_sq.map(x => x.zipWithIndex.map(y => (returnMapping3((y._1.abs, y._2), cases.toArray), y._1 < 0)))
    var mult_layer_count1 = 0
    var cases_no_mult = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re)._1 || !FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im)._1) {
        mult_layer_count1 += 1
      } else {
        cases_no_mult += cases_addr_adjusted(i)
      }
    }
    var subadd_layer_count1 = 0
    var cases_no_add = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.abs < 0.00005) && !(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.abs < 0.00005)) {
        subadd_layer_count1 += 1
      } else {
        cases_no_add += cases_addr_adjusted(i)
      }
    }
    val CMultLatency = 2
    val CAddLatency = 1
    val initial_Latency = 1
    val multlayerlatency = if (mult_layer_count1 > 0) {
      1
    } else {
      0
    }
    val subaddlayerlatency = if (subadd_layer_count1 > 0) {
      1
    } else {
      0
    }
    val multadd_latency = if (inner_inner_square_size > 1) {
      ((Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt + (for (l <- 0 until (Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt) yield {
        (inner_inner_square_size / Math.pow(2, l)).floor.toInt % 2
      }).reduce(_ + _)) * (CAddLatency)
    } else {
      0
    }
    val end_latency = 1
    var DFT_latency = initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency + end_latency
    if (r == 2) {
      DFT_latency = 1
    }
    val DFTs_per_stage = N / r
    val number_of_stages = (Math.log10(N) / Math.log10(r)).round.toInt
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    var T_L = 0
    for (i <- 0 until TotalStages) {
      val twid = Permutations.T2(N, r)(i)
      var mult_count2 = 0
      for (i <- 0 until w) {
        val n = twid(i)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
          mult_count2 += 1
        }
      }
      var T_latency = CMultLatency
      if (mult_count2 == 0) {
        T_latency = 0
      }
      T_L += T_latency
    }
    val Twid_latency = (N / w) * CMultLatency
    val Perm_latency = 0
    val Total_Latency = T_L + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency + 1
    val DFT_instances = (for (i <- 0 until number_of_stages) yield {
      val DFT_instnace_row = (for (j <- 0 until DFTs_per_stage) yield {
        val DFT_instance = Module(new DFT_Symmetric_NRV_v2(r, bw)).io
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
      val Twid = Module(new TwiddleFactors_v2(N, r, N, i, bw)).io
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

  class FFT_SingleRadix_Streaming_NRO_v2(N: Int, r: Int, w: Int, bw: Int) extends Module { // streaming single radix fft, still in progress
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_ready = Input(Bool())
      val out_validate = Output(Bool())
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val DFTr_Constants = FFT.DFT_gen(r).map(_.toVector).toVector
    var mult_count = 0
    for (i <- 0 until r - 1) {
      for (j <- 0 until r - 1) {
        val n = DFTr_Constants(i + 1)(j + 1)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
          mult_count += 1
        }
      }
    }
    val inner_square_size = r - 1
    var last_ind = r - 1
    var start_ind = 1
    val inner_inner_square_size = inner_square_size / 2
    val ind_sq = for (i <- 0 until inner_inner_square_size) yield {
      val row = for (j <- 0 until inner_inner_square_size) yield {
        if (((i + 1) * (j + 1) % r) > r / 2) {
          ((i + 1) * (j + 1) % r) - r
        } else {
          ((i + 1) * (j + 1) % r)
        }
      }
      row
    }
    val ind_sq_unique = mutable.ArrayBuffer[Int]()
    val ind_sq_unique_address = mutable.ArrayBuffer[(Int, Int)]()
    val cases = mutable.ArrayBuffer[(Int, Int)]()
    val cases_addr = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until inner_inner_square_size) {
      for (j <- 0 until inner_inner_square_size) {
        if (!ind_sq_unique.contains(ind_sq(i)(j).abs)) {
          ind_sq_unique += ind_sq(i)(j)
          val temp = (i, j)
          ind_sq_unique_address += temp
        }
        if (!cases.contains((ind_sq(i)(j).abs, j))) {
          val temp = (ind_sq(i)(j).abs, j)
          cases += temp
          val temp2 = (i, j)
          cases_addr += temp2
        }
      }
    }

    def returnMapping2(num: Int, arr: Array[(Int, Int)]): (Int, Int) = { //for multiply access values
      var returnval = (0, 0)
      for (i <- 0 until arr.length) {
        if (num == arr(i)._1) {
          returnval = ind_sq_unique_address(i)
        }
      }
      returnval
    }

    val cases_addr_adjusted = cases.map(x => returnMapping2(x._1.abs, ind_sq_unique.zipWithIndex.toArray))

    def returnMapping3(num: (Int, Int), arr: Array[(Int, Int)]): Int = {
      var returnval = 0
      for (i <- 0 until arr.length) {
        if (num == arr(i)) {
          returnval = i
        }
      }
      returnval
    }

    val new_adj_case_sq = ind_sq.map(x => x.zipWithIndex.map(y => (returnMapping3((y._1.abs, y._2), cases.toArray), y._1 < 0)))
    var mult_layer_count1 = 0
    var cases_no_mult = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re)._1 || !FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im)._1) {
        mult_layer_count1 += 1
      } else {
        cases_no_mult += cases_addr_adjusted(i)
      }
    }
    var subadd_layer_count1 = 0
    var cases_no_add = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.abs < 0.00005) && !(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.abs < 0.00005)) {
        subadd_layer_count1 += 1
      } else {
        cases_no_add += cases_addr_adjusted(i)
      }
    }
    val CMultLatency = 2
    val CAddLatency = 1
    val initial_Latency = 1
    val multlayerlatency = if (mult_layer_count1 > 0) {
      1
    } else {
      0
    }
    val subaddlayerlatency = if (subadd_layer_count1 > 0) {
      1
    } else {
      0
    }
    val multadd_latency = if (inner_inner_square_size > 1) {
      ((Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt + (for (l <- 0 until (Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt) yield {
        (inner_inner_square_size / Math.pow(2, l)).floor.toInt % 2
      }).reduce(_ + _)) * (CAddLatency)
    } else {
      0
    }
    val end_latency = 1
    var DFT_latency = initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency + end_latency
    if (r == 2) {
      DFT_latency = 1
    }
    val DFTs_per_stage = w / r
    val number_of_stages = (Math.log10(N) / Math.log10(r)).round.toInt
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    var T_L = CMultLatency
    val Twid_latency = TotalStages * T_L
    val Perm_latency = (N / w) * 2
    val Total_Latency = Twid_latency + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency + 1
    val DFT_regdelays = RegInit(VecInit.fill(number_of_stages)(VecInit.fill(DFT_latency)(false.B)))
    val Twid_regdelays = RegInit(VecInit.fill(number_of_stages - 1)(VecInit.fill(T_L)(false.B)))
    val Perm_regdelays = RegInit(VecInit.fill(number_of_stages + 1)(VecInit.fill(Perm_latency)(false.B)))
    val DFT_modules = for (i <- 0 until number_of_stages) yield {
      val row = for (j <- 0 until w / r) yield {
        val instance = Module(new DFT_Symmetric_NRV_v2(r, bw)).io
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
      val instance = Module(new TwiddleFactorsStreamed_v2(N, r, w, i, bw)).io
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
