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
import TwidFactorDesigns._
import PermutationDesigns._
import scala.collection.mutable

object DFTDesigns {
  // the v2 versions have an enable input

  class DFT_Symmetric_NRV_v2(r: Int, bw: Int, inv: Boolean) extends Module {
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in = Input(Vec(r, new ComplexNum(bw)))
      val out = Output(Vec(r, new ComplexNum(bw)))
    })
    val DFTr_Constants = if(inv){FFT.inv_DFT_gen(r).map(_.toVector).toVector}else{FFT.DFT_gen(r).map(_.toVector).toVector}
    val is_odd = if (r % 2 == 0) {
      false
    } else {
      true
    }
    if (r == 2) {
      val dft2 = Module(new DFT_NRV_V2(2, bw, inv)).io
      dft2.in_en := io.in_en
      dft2.in := io.in
      io.out := dft2.out
    }
    else if (is_odd) {
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
      val initial_adds_subs = for (i <- 0 until inner_inner_square_size) yield {
        val ias = for (j <- 0 until 2) yield {
          if (j == 0) {
            val instance = Module(new FPComplexAdder_v2(bw)).io
            instance
          } else {
            val instance = Module(new FPComplexSub_v2(bw)).io
            instance
          }
        }
        ias
      }
      for (i <- 0 until inner_inner_square_size) {
        initial_adds_subs(i)(0).in_en := io.in_en
        initial_adds_subs(i)(1).in_en := io.in_en
        initial_adds_subs(i)(0).in_a := io.in(start_ind)
        initial_adds_subs(i)(0).in_b := io.in(last_ind)
        initial_adds_subs(i)(1).in_a := io.in(start_ind)
        initial_adds_subs(i)(1).in_b := io.in(last_ind)
        start_ind += 1
        last_ind -= 1
      }
      val top_row_init_sum = Module(new FPComplexMultiAdder_v2(inner_inner_square_size, bw)).io
      top_row_init_sum.in_en := io.in_en
      val initial_layer_out = if (subaddlayerlatency + multlayerlatency > 0) {
        RegInit(VecInit.fill(subaddlayerlatency + multlayerlatency)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      } else {
        WireInit(VecInit.fill(2)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      }
      val slctlncy = if (subaddlayerlatency + multlayerlatency > 0) {
        subaddlayerlatency + multlayerlatency
      } else {
        2
      }
      when(io.in_en) {
        for (i <- 0 until subaddlayerlatency + multlayerlatency) {
          if (i == 0) {
            for (j <- 0 until inner_inner_square_size) {
              initial_layer_out(0)(j) := initial_adds_subs(j)(0).out_s
            }
          } else {
            initial_layer_out(i) := initial_layer_out(i - 1)
          }
        }
      }
      top_row_init_sum.in := initial_layer_out(slctlncy - 1)
      val initial_mults = for (i <- 0 until cases.length) yield {
        val mults = for (j <- 0 until 2) yield {
          if (j == 0) {
            val mult_instance = Module(new FPComplexMult_reducable_forSymmetric_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, 0.0, if (mult_layer_count1 > 0) {
              true
            } else {
              false
            }, true)).io // these will at most be 1 regular mult
            mult_instance
          } else {
            val mult_instance = Module(new FPComplexMult_reducable_forSymmetric_v2(bw, 0.0, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (mult_layer_count1 > 0) {
              true
            } else {
              false
            }, true)).io
            mult_instance
          }
        }
        mults
      }

      for (i <- 0 until cases.length) {
        initial_mults(i)(0).in_en := io.in_en
        initial_mults(i)(1).in_en := io.in_en
        initial_mults(i)(0).in_a := initial_adds_subs(cases_addr(i)._2)(0).out_s
        initial_mults(i)(0).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.toString, bw).U
        initial_mults(i)(0).in_b.Im := 0.U
        initial_mults(i)(1).in_a := initial_adds_subs(cases_addr(i)._2)(1).out_s
        initial_mults(i)(1).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.toString, bw).U
        initial_mults(i)(1).in_b.Re := 0.U
      }
      val stage_adds_subs = for (i <- 0 until cases.length) yield {
        val instances = for (j <- 0 until 2) yield {
          if (j == 0) {
            val instance = Module(new FPComplexAdder_reducable_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (subadd_layer_count1 > 0) {
              true
            } else {
              false
            })).io
            instance
          } else {
            val instance = Module(new FPComplexSub_reducable_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (subadd_layer_count1 > 0) {
              true
            } else {
              false
            })).io
            instance
          }
        }
        instances
      }
      for (i <- 0 until cases.length) {
        stage_adds_subs(i)(0).in_en := io.in_en
        stage_adds_subs(i)(1).in_en := io.in_en
        stage_adds_subs(i)(0).in_a := initial_mults(i)(0).out_s
        stage_adds_subs(i)(0).in_b := initial_mults(i)(1).out_s
        stage_adds_subs(i)(1).in_a := initial_mults(i)(0).out_s
        stage_adds_subs(i)(1).in_b := initial_mults(i)(1).out_s
      }

      val fillIn = WireInit(VecInit.fill(inner_inner_square_size)(VecInit.fill(inner_inner_square_size)(VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw))))))
      for (i <- 0 until inner_inner_square_size) {
        for (j <- 0 until inner_inner_square_size) {
          if (new_adj_case_sq(i)(j)._2) {
            fillIn(i)(j)(0) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(1).out_s
            fillIn(i)(j)(1) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(0).out_s
          } else {
            fillIn(i)(j)(0) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(0).out_s
            fillIn(i)(j)(1) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(1).out_s
          }
        }
      }

      val fpmultiadds = for (i <- 0 until inner_square_size) yield {
        val fpmadds = Module(new FPComplexMultiAdder_v2(inner_inner_square_size, bw)).io
        fpmadds
      }
      for(i <- 0 until inner_square_size){
        fpmultiadds(i).in_en := io.in_en
      }

      for (i <- 0 until 2) {
        for (j <- 0 until inner_inner_square_size) {
          for (k <- 0 until inner_inner_square_size) {
            if (i == 0) {
              fpmultiadds(j).in(k) := fillIn(j)(k)(i)
            } else {
              fpmultiadds(inner_square_size - j - 1).in(k) := fillIn(j)(k)(i)
            }
          }
        }
      }
      val results = WireInit(VecInit.fill(r)(0.U.asTypeOf(new ComplexNum(bw))))
      for (i <- 0 until r) {
        if (i == 0) {
          results(0) := top_row_init_sum.out
        } else {
          results(i) := fpmultiadds(i - 1).out
        }
      }
      val final_adder_layer = for (i <- 0 until r) yield {
        val adder = Module(new FPComplexAdder_v2(bw)).io
        adder
      }
      for(i <- 0 until r){
        final_adder_layer(i).in_en := io.in_en
      }
      val finallayer = RegInit(VecInit.fill(initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency)(0.U.asTypeOf(new ComplexNum(bw))))
      when(io.in_en) {
        for (i <- 0 until initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency) {
          if (i == 0) {
            finallayer(0) := io.in(0)
          } else {
            finallayer(i) := finallayer(i - 1)
          }
        }
      }
      for (i <- 0 until r) {
        final_adder_layer(i).in_a := finallayer(initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency - 1)
        final_adder_layer(i).in_b := results(i)
        io.out(i) := final_adder_layer(i).out_s
      }
    } else {
      val inner_square_size = r - 2
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
      val initial_adds_subs = for (i <- 0 until inner_inner_square_size) yield {
        val ias = for (j <- 0 until 2) yield {
          if (j == 0) {
            val instance = Module(new FPComplexAdder_v2(bw)).io
            instance
          } else {
            val instance = Module(new FPComplexSub_v2(bw)).io
            instance
          }
        }
        ias
      }
      for (i <- 0 until inner_inner_square_size) {
        initial_adds_subs(i)(0).in_en := io.in_en
        initial_adds_subs(i)(1).in_en := io.in_en
        initial_adds_subs(i)(0).in_a := io.in(start_ind)
        initial_adds_subs(i)(0).in_b := io.in(last_ind)
        initial_adds_subs(i)(1).in_a := io.in(start_ind)
        initial_adds_subs(i)(1).in_b := io.in(last_ind)
        start_ind += 1
        last_ind -= 1
      }

      val top_row_init_sum = Module(new FPComplexMultiAdder_v2(inner_inner_square_size, bw)).io
      val mid_row_init_sum = Module(new FPComplexMultiAdder_v2(inner_inner_square_size, bw)).io
      top_row_init_sum.in_en := io.in_en
      mid_row_init_sum.in_en := io.in_en
      var switch = true
      val initial_layer_out = if (subaddlayerlatency + multlayerlatency > 0) {
        RegInit(VecInit.fill(subaddlayerlatency + multlayerlatency)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      } else {
        WireInit(VecInit.fill(2)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      }
      val initial_layer_out_mid = if (subaddlayerlatency + multlayerlatency > 0) {
        RegInit(VecInit.fill(subaddlayerlatency + multlayerlatency)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      } else {
        WireInit(VecInit.fill(2)(VecInit.fill(inner_inner_square_size)(0.U.asTypeOf(new ComplexNum(bw)))))
      }
      val slctlncy = if (subaddlayerlatency + multlayerlatency > 0) {
        subaddlayerlatency + multlayerlatency
      } else {
        2
      }
      when(io.in_en) {
        for (i <- 0 until subaddlayerlatency + multlayerlatency) {
          if (i == 0) {
            for (j <- 0 until inner_inner_square_size) {
              initial_layer_out(0)(j) := initial_adds_subs(j)(0).out_s
              if (switch) {
                initial_layer_out_mid(0)(j).Re := (~initial_adds_subs(j)(0).out_s.Re(bw - 1)) ## initial_adds_subs(j)(0).out_s.Re(bw - 2, 0)
                initial_layer_out_mid(0)(j).Im := (~initial_adds_subs(j)(0).out_s.Im(bw - 1)) ## initial_adds_subs(j)(0).out_s.Im(bw - 2, 0)
                switch = false
              } else {
                initial_layer_out_mid(0)(j) := initial_adds_subs(j)(0).out_s
                switch = true
              }
            }
          } else {
            initial_layer_out(i) := initial_layer_out(i - 1)
            initial_layer_out_mid(i) := initial_layer_out_mid(i - 1)
          }
        }
      }
      top_row_init_sum.in := initial_layer_out(slctlncy - 1)
      mid_row_init_sum.in := initial_layer_out_mid(slctlncy - 1)
      val initial_mults = for (i <- 0 until cases.length) yield {
        val mults = for (j <- 0 until 2) yield {
          if (j == 0) {
            val mult_instance = Module(new FPComplexMult_reducable_forSymmetric_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, 0.0, if (mult_layer_count1 > 0) {
              true
            } else {
              false
            }, true)).io
            mult_instance
          } else {
            val mult_instance = Module(new FPComplexMult_reducable_forSymmetric_v2(bw, 0.0, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (mult_layer_count1 > 0) {
              true
            } else {
              false
            }, true)).io
            mult_instance
          }
        }
        mults
      }

      for (i <- 0 until cases.length) {
        initial_mults(i)(0).in_en := io.in_en
        initial_mults(i)(1).in_en := io.in_en
        initial_mults(i)(0).in_a := initial_adds_subs(cases_addr(i)._2)(0).out_s
        initial_mults(i)(0).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.toString, bw).U
        initial_mults(i)(0).in_b.Im := 0.U
        initial_mults(i)(1).in_a := initial_adds_subs(cases_addr(i)._2)(1).out_s
        initial_mults(i)(1).in_b.Re := 0.U
        initial_mults(i)(1).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.toString, bw).U
      }
      val stage_adds_subs = for (i <- 0 until cases.length) yield {
        val instances = for (j <- 0 until 2) yield {
          if (j == 0) {
            val instance = Module(new FPComplexAdder_reducable_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (subadd_layer_count1 > 0) {
              true
            } else {
              false
            })).io
            instance
          } else {
            val instance = Module(new FPComplexSub_reducable_v2(bw, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re, DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im, if (subadd_layer_count1 > 0) {
              true
            } else {
              false
            })).io
            instance
          }
        }
        instances
      }

      for (i <- 0 until cases.length) {
        stage_adds_subs(i)(0).in_en := io.in_en
        stage_adds_subs(i)(1).in_en := io.in_en
        stage_adds_subs(i)(0).in_a := initial_mults(i)(0).out_s
        stage_adds_subs(i)(0).in_b := initial_mults(i)(1).out_s
        stage_adds_subs(i)(1).in_a := initial_mults(i)(0).out_s
        stage_adds_subs(i)(1).in_b := initial_mults(i)(1).out_s
      }

      val fillIn = WireInit(VecInit.fill(inner_inner_square_size)(VecInit.fill(inner_inner_square_size)(VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw))))))
      for (i <- 0 until inner_inner_square_size) {
        for (j <- 0 until inner_inner_square_size) {
          if (new_adj_case_sq(i)(j)._2) {
            fillIn(i)(j)(0) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(1).out_s
            fillIn(i)(j)(1) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(0).out_s
          } else {
            fillIn(i)(j)(0) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(0).out_s
            fillIn(i)(j)(1) := stage_adds_subs(new_adj_case_sq(i)(j)._1)(1).out_s
          }
        }
      }
      val fpmultiadds = for (i <- 0 until inner_square_size) yield {
        val fpmadds = Module(new FPComplexMultiAdder_v2(inner_inner_square_size, bw)).io
        fpmadds
      }
      for(i <- 0 until inner_square_size){
        fpmultiadds(i).in_en := io.in_en
      }
      for (i <- 0 until 2) {
        for (j <- 0 until inner_inner_square_size) {
          for (k <- 0 until inner_inner_square_size) {
            if (i == 0) {
              fpmultiadds(j).in(k) := fillIn(j)(k)(i)
            } else {
              fpmultiadds(inner_square_size - j - 1).in(k) := fillIn(j)(k)(i)
            }
          }
        }
      }
      val results = WireInit(VecInit.fill(r)(0.U.asTypeOf(new ComplexNum(bw))))
      var offst = 0
      for (i <- 0 until r) {
        if (i == 0) {
          results(0) := top_row_init_sum.out
        } else if (i == r / 2) {
          results(r / 2) := mid_row_init_sum.out
          offst = 1
        } else {
          results(i) := fpmultiadds(i - 1 - offst).out
        }
      }
      val prep_adder_layer = for (i <- 0 until 2) yield {
        if (i == 0) {
          val adder = Module(new FPComplexAdder_v2(bw)).io
          adder
        } else {
          val adder = Module(new FPComplexSub_v2(bw)).io
          adder
        }
      }
      for (i <- 0 until 2) {
        prep_adder_layer(i).in_en := io.in_en
        prep_adder_layer(i).in_a := io.in(0)
        prep_adder_layer(i).in_b := io.in(r / 2)
      }
      val finallayer = if (multlayerlatency + subaddlayerlatency + multadd_latency > 0) {
        RegInit(VecInit.fill(multlayerlatency + subaddlayerlatency + multadd_latency)(VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw)))))
      } else {
        WireInit(VecInit.fill(2)(VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw)))))
      }
      val slctlncy2 = if (subaddlayerlatency + multlayerlatency + multadd_latency > 0) {
        subaddlayerlatency + multlayerlatency + multadd_latency
      } else {
        2
      }
      when(io.in_en) {
        for (i <- 0 until slctlncy2) {
          if (i == 0) {
            for (j <- 0 until 2) {
              finallayer(0)(j) := prep_adder_layer(j).out_s
            }
          } else {
            finallayer(i) := finallayer(i - 1)
          }
        }
      }
      for (i <- 0 until r) {}
      val final_adder_layer = for (i <- 0 until r) yield {
        val adder = Module(new FPComplexAdder_v2(bw)).io
        adder
      }
      for (i <- 0 until r) {
        final_adder_layer(i).in_en := io.in_en
        final_adder_layer(i).in_a := finallayer(slctlncy2 - 1)(i % 2) //io.in(0)
        final_adder_layer(i).in_b := results(i)
        io.out(i) := final_adder_layer(i).out_s
      }
    }
  }

  class DFT_NRV_V2(r: Int, bw: Int, inv: Boolean) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(r, new ComplexNum(bw)))
      val in_en = Input(Bool())
      val out = Output(Vec(r, new ComplexNum(bw)))
    })
    val DFTr_Constants = if(inv){FFT.inv_DFT_gen(r).map(_.toVector).toVector}else{FFT.DFT_gen(r).map(_.toVector).toVector}
    var mult_count = 0
    val mult_ind = mutable.ArrayBuffer[Int]()
    val mult_needs = for (i <- 0 until r - 1) yield {
      val multiplier_ind = for (j <- 0 until r - 1) yield {
        val n = DFTr_Constants(i + 1)(j + 1)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.00005) || (c2._1 && n.re.abs < 0.00005))) {
          mult_count += 1
          mult_ind += (i * (r - 1) + j)
          (i, j, 0, false, false, true)
        } else {
          mult_ind += (i * (r - 1) + j)
          if ((c1._1 && n.im.abs < 0.00005)) {
            (i, j, c1._2.abs, n.re < -0.0005, false, false)
          } else {
            (i, j, c2._2.abs, n.im < -0.0005, true, false)
          }
        }
      }
      multiplier_ind.toIndexedSeq
    }
    val CMultLatency = 2
    val CAddLatency = 1
    var DFT_latency = CMultLatency + ((Math.log10(r) / Math.log10(2)).floor.toInt + (for (l <- 0 until (Math.log10(r) / Math.log10(2)).floor.toInt) yield {
      ((r / Math.pow(2, l)).floor.toInt) % 2
    }).reduce(_ + _)) * (CAddLatency)
    if (mult_count == 0) {
      DFT_latency = ((Math.log10(r) / Math.log10(2)).floor.toInt + (for (l <- 0 until (Math.log10(r) / Math.log10(2)).floor.toInt) yield {
        (r / Math.pow(2, l)).floor.toInt % 2
      }).reduce(_ + _)) * (CAddLatency)
    }
    val adj = mutable.ArrayBuffer[(Int, Int, Int, Boolean, Boolean, Boolean)]()
    val mult_ind_adj = mutable.ArrayBuffer[Int]()
    val adj_non_mult = mutable.ArrayBuffer[(Int, Int, Int, Boolean, Boolean, Boolean)]()
    val mult_ind_adj_non_mult = mutable.ArrayBuffer[Int]()
    for (i <- 0 until r - 1) {
      for (j <- 0 until r - 1) {
        if (mult_needs(i)(j)._6) {
          adj += mult_needs(i)(j)
          mult_ind_adj += mult_ind(i * (r - 1) + j)
        } else {
          adj_non_mult += mult_needs(i)(j)
          mult_ind_adj_non_mult += mult_ind(i * (r - 1) + j)
        }
      }
    }




    var cmplx_adjusts = for (i <- 0 until (r - 1) * (r - 1) - mult_count) yield {
      val cm = Module(new ComplexNum_AdjustOrder_v2(bw)).io
      cm
    }
    val adj_wire = Wire(Vec((r - 1) * (r - 1), new ComplexNum(bw)))
    for (i <- 0 until (r - 1) * (r - 1) - mult_count) {
      cmplx_adjusts(i).in_en := io.in_en
      cmplx_adjusts(i).in := io.in(adj_non_mult(i)._2 + 1)
      cmplx_adjusts(i).in_adj := adj_non_mult(i)._3.U
      cmplx_adjusts(i).is_neg := adj_non_mult(i)._4.B
      cmplx_adjusts(i).is_flip := adj_non_mult(i)._5.B
      adj_wire(mult_ind_adj_non_mult(i)) := cmplx_adjusts(i).out
    }
    for (i <- 0 until mult_count) {
      adj_wire(mult_ind_adj(i)) := io.in(adj(i)._2 + 1)
    }
    val FP_adds = (for (i <- 0 until r) yield {
      val fpadds = Module(new FPComplexMultiAdder_v2(r, bw)).io
      fpadds
    }).toVector
    for(i <- 0 until r){
      FP_adds(i).in_en := io.in_en
    }
    if (mult_count != 0) {
      var FP_Mults = (for (i <- 0 until mult_count) yield {
        val fpmult = Module(new FPComplexMult_reducable_SimpleCases_v2(bw, DFTr_Constants(adj(i)._1 + 1)(adj(i)._2 + 1).re, DFTr_Constants(adj(i)._1 + 1)(adj(i)._2 + 1).im)).io
        fpmult
      })
      for (k <- 0 until mult_count) {
        FP_Mults(k).in_en := io.in_en
        FP_Mults(k).in_b.Re := convert_string_to_IEEE_754(DFTr_Constants(adj(k)._1 + 1)(adj(k)._2 + 1).re.toString, bw).U
        FP_Mults(k).in_a := adj_wire(mult_ind_adj(k)) //io.in(mult_ind_adj(k))
        FP_Mults(k).in_b.Im := convert_string_to_IEEE_754(DFTr_Constants(adj(k)._1 + 1)(adj(k)._2 + 1).im.toString, bw).U
      }
      val total_reg = r + ((r - 1) * (r - 1) - mult_count)
      val reg_syncs = (for (i <- 0 until CMultLatency) yield {
        val regs = RegInit(VecInit.fill(total_reg)(0.U.asTypeOf(new ComplexNum(bw))))
        regs
      }).toVector
      when(io.in_en) {
        for (i <- 0 until CMultLatency) {
          if (i == 0) {
            for (j <- 0 until r) {
              reg_syncs(0)(j) := io.in(j)
            }
            for (j <- 0 until ((r - 1) * (r - 1)) - mult_count) {
              reg_syncs(0)(j + r) := adj_wire(mult_ind_adj_non_mult(j))
            }
          } else {
            reg_syncs(i) := reg_syncs(i - 1)
          }
        }
      }
      val mult_results = Wire(Vec(r, Vec(r, new ComplexNum(bw))))
      for (i <- 0 until r) {
        mult_results(0)(i) := reg_syncs(CMultLatency - 1)(i)
        if (i != 0)
          mult_results(i)(0) := reg_syncs(CMultLatency - 1)(0)
      }
      for (i <- 0 until ((r - 1) * (r - 1)) - mult_count) {
        mult_results(adj_non_mult(i)._1 + 1)(adj_non_mult(i)._2 + 1) := reg_syncs(CMultLatency - 1)(i + r)
      }
      for (i <- 0 until mult_count) {
        mult_results(adj(i)._1 + 1)(adj(i)._2 + 1) := FP_Mults(i).out_s
      }
      for (i <- 0 until r) {
        for (j <- 0 until r) {
          FP_adds(i).in(j) := mult_results(i)(j)
        }
      }
    } else {
      for (i <- 0 until r) {
        if (i == 0) {
          for (j <- 0 until r) {
            FP_adds(i).in(j) := io.in(j)
          }
        } else {
          for (j <- 0 until r) {
            if (j == 0) {
              FP_adds(i).in(0) := io.in(0)
            } else {
              FP_adds(i).in(j) := adj_wire((i - 1) * (r - 1) + j - 1)
            }
          }
        }
      }
    }
    val result = WireInit(VecInit.fill(r)(0.U.asTypeOf(new ComplexNum(bw))))
    for (i <- 0 until r) {
      result(i) := FP_adds(i).out
    }

    for (i <- 0 until r) {
      io.out(i) := result(i)
    }
  }
}
